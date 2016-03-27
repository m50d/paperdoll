package paperdoll.arm

import resource.Resource
import paperdoll.core.effect.Eff
import resource.ManagedResource
import resource.managed
import scala.reflect.Manifest
import scalaz.Leibniz
import paperdoll.core.layer.Member
import paperdoll.core.layer.Layers
import shapeless.Coproduct
import scalaz.Leibniz.===
import shapeless.UnaryTCConstraint
import paperdoll.core.effect.Pure
import scalaz.Forall
import paperdoll.core.effect.Arr_
import paperdoll.core.effect.Arrs
import paperdoll.core.effect.Impure
import paperdoll.core.queue.Queue
import paperdoll.core.effect.Bind
import paperdoll.core.effect.Arr
import paperdoll.core.effect.Handler
import scalaz.syntax.monad._
import Predef.identity

sealed trait Region[S, R, A] {
  def fold[B](resource: (A === R, ManagedResource[R]) => B): B
}

object Region {
  def newSHandle[R: Resource: Manifest](r: => R): Eff.One[Region_[S, R], R]#O forSome { type S } =
    Eff.send[Region_[Any, R], R](new Region[Any, R, R] {
      override def fold[B](resource: (R === R, ManagedResource[R]) => B) =
        resource(Leibniz.refl, managed(r))
    })

  private[this] def handleInRgn[S, RE] = Eff.handle(new Bind[Region_[S, RE]] {
    override type O[X] = Either[Seq[Throwable], X]
    override def pure[A](a: A) = Right(a)
    override def apply[V, RR <: Coproduct, RL <: Layers[RR], A](eff: Region[S, RE, V], cont: Arr[RR, RL, V, O[A]]) =
      Pure(Left(Seq(new RuntimeException("Opened the same handle twice. The type system is supposed to forbid this"))))
  })

  def newRgn[S, RE]: Handler[Region_[S, RE]] = new Handler[Region_[S, RE]] {
    override type O[X] = Either[Seq[Throwable], X]
    override def run[R <: Coproduct, L1 <: Layers[R], A](eff: Eff[R, L1, A])(
      implicit me: Member[R, Region_[S, RE]] { type L = L1 }): Eff[me.RestR, me.RestL, O[A]] =
      eff.fold(
        a => Pure[me.RestR, me.RestL, O[A]](Right(a)),
        new Forall[({ type K[X] = (me.L#O[X], Arrs[R, me.L, X, A]) ⇒ Eff[me.RestR, me.RestL, O[A]] })#K] {
          override def apply[X] = { (eff, cont) ⇒
            val composed = Eff.compose(cont)
            me.remove(eff).fold(
              otherEffect ⇒ Impure[me.RestR, me.RestL, X, O[A]](otherEffect, Queue.one[Arr_[me.RestR, me.RestL]#O, X, O[A]](
                composed andThen { run(_) })),
              _.fold({ (le, res) =>
                res.acquireFor {
                  re =>
                    //FIXME I think this is still lazy. We may need to use a lower-level interface
                    //than ManagedResource: open the resource explicitly, and then close it in the final
                    //pure case
                    handleInRgn(le.subst[({ type K[Y] = Arr[R, L1, Y, A] })#K](composed)(re))
                }.fold(
                  fails => Pure(Left(fails)),
                  identity)
              }))
          }
        })
  }
}