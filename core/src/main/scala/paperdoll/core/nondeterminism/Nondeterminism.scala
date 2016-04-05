package paperdoll.core.nondeterminism

import paperdoll.core.layer.Layers
import shapeless.{:+:, Coproduct}
import paperdoll.core.effect.Impure
import paperdoll.core.effect.Effects
import paperdoll.core.effect.Pure
import scalaz.Forall
import scalaz.Leibniz
import shapeless.CNil
import scalaz.syntax.foldable._
import scalaz.std.list._
import scalaz.syntax.std.list._
import paperdoll.core.queue.Queue
import paperdoll.core.effect.Arr
import scalaz.syntax.monad._
import paperdoll.core.effect.Bind
import paperdoll.core.effect.Handler
import scalaz.Foldable
import scalaz.Unapply
import scala.Vector
import scalaz.Leibniz.===
import paperdoll.core.effect.Arrs
import paperdoll.core.effect.Effects_
import paperdoll.core.layer.Member
import paperdoll.core.layer.Subset
import paperdoll.core.effect.Arr_

sealed trait Nondeterminism[A] {
  def fold[B](zero: ⇒ B, plus: A === Boolean ⇒ B): B
}

object Nondeterminism {
  private[this] def zero[A] = new Nondeterminism[A] {
    override def fold[B](zero: ⇒ B, plus: A === Boolean ⇒ B) = zero
  }
  private[this] def plus = new Nondeterminism[Boolean] {
    override def fold[B](zero: ⇒ B, plus: Boolean === Boolean ⇒ B) = plus(Leibniz.refl)
  }
  def Zero[A] = Effects.send[NDet_, A](zero)
  def Plus = Effects.send[NDet_, Boolean](plus)
  def collapse[F[_]: Foldable, A](fa: F[A]): Effects.One[NDet_, A] =
    fa.collapse[Effects.One_[NDet_]#O]
  def collapseU[FA](fa: FA)(implicit u: Unapply[Foldable, FA]): Effects.One[NDet_, u.A] =
    collapse(u.leibniz(fa))(u.TC)
  
  private[this] def loop[R <: Coproduct, L0 <: Layers[R], A](
    jq: List[Effects[R, L0, A]], j: Effects[R, L0, A])(implicit su: Subset[R, NDet_ :+: CNil] {
      type LS = L0
      type LT = Layers.One[NDet_]
    }, me: Member[R, NDet_] { type L = L0 }): Effects[R, L0, Option[(A, Effects[R, L0, A])]] =
    j.fold(a => Pure[R, L0, Option[(A, Effects[R, L0, A])]](Some((a, jq.msuml[Effects_[R, L0]#O, A]))),
      new Forall[({ type K[X] = (L0#O[X], Arrs[R, L0, X, A]) => Effects[R, L0, Option[(A, Effects[R, L0, A])]] })#K] {
        override def apply[X] = {
          (eff, cont) =>
            me.remove(eff).fold({ otherEffect =>
              val newCont = Effects.compose(cont) andThen { loop(jq, _) }
              //We pass eff here, knowing that it is now really otherEffects.
              //Arguably it would be more correct to lift otherEffects back into the [R, L0]
              //layer stack using .extend (or some inverse method on Member)
              //but that would add overhead for no practical benefit
              Impure[R, L0, X, Option[(A, Effects[R, L0, A])]](
                eff, Queue.one[Arr_[R, L0]#O, X, Option[(A, Effects[R, L0, A])]](newCont))
            },
              _.fold({
                jq.toNel.fold[Effects[R, L0, Option[(A, Effects[R, L0, A])]]](Pure[R, L0, Option[(A, Effects[R, L0, A])]](None))({
                  jqn => loop(jqn.tail.toList, jqn.head)
                })
              }, { le =>
                val booleanCont = Effects.compose(le.subst[({ type K[Y] = Arrs[R, L0, Y, A] })#K](cont))
                loop(booleanCont(false) :: jq, booleanCont(true))
              }))
        }
      })

  def msplit[R <: Coproduct, L0 <: Layers[R], A, L1 <: Layers[NDet_ :+: CNil], L2 <: Layers[R]](eff: Effects[R, L0, A])(
    implicit su: Subset[R, NDet_ :+: CNil] {
      type LS = L0
      type LT = L1
    }, me: Member[R, NDet_] { type L = L2 },
    le1: Leibniz[Nothing, Layers[NDet_ :+: CNil], L1, Layers.One[NDet_]],
    le2: Leibniz[Nothing, Layers[R], L2, L0]): Effects[R, L0, Option[(A, Effects[R, L0, A])]] =
    loop(Nil, eff)(le1.subst[({
      type K[LL <: Layers[NDet_ :+: CNil]] = Subset[R, NDet_ :+: CNil] {
        type LS = L0
        type LT = LL
      }
    })#K](su), le2.subst[({
      type K[LL <: Layers[R]] = Member[R, NDet_] {
        type L = LL
      }
    })#K](me))

  def ifte[R <: Coproduct, L0 <: Layers[R], A, B, L1 <: Layers[NDet_ :+: CNil], L2 <: Layers[R]](
      t: Effects[R, L0, A], th: Arr[R, L0, A, B], el: Effects[R, L0, B])(
    implicit su: Subset[R, NDet_ :+: CNil] {
      type LS = L0
      type LT = L1
    }, me: Member[R, NDet_] { type L = L2 },
    le1: Leibniz[Nothing, Layers[NDet_ :+: CNil], L1, Layers.One[NDet_]],
    le2: Leibniz[Nothing, Layers[R], L2, L0]): Effects[R, L0, B] =
    msplit(t).flatMap(_.fold(el)(u => th(u._1)))
    
  /**
   * Example interpreter. The paper implements this for any F[_]: Alternative
   * rather than just Vector 
   */
  def runNDetVector: Handler.Aux[NDet_, Vector] = Effects.handle(new Bind[NDet_]{
    override type O[X] = Vector[X]
    override def pure[A](a: A) = Vector(a)
    override def apply[V, RR <: Coproduct, RL <: Layers[RR], A](eff: Nondeterminism[V], cont: Arr[RR, RL, V, Vector[A]]) =
      eff.fold(Pure(Vector()), {
        le =>
          val booleanCont = le.subst[({ type K[Y] = Arr[RR, RL, Y, Vector[A]] })#K](cont)
          for {
            x <- booleanCont(true)
            y <- booleanCont(false)
          } yield x ++ y
      })
  })
}