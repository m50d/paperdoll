package paperdoll.arm

import resource.Resource
import paperdoll.core.effect.Effects
import scalaz.Leibniz
import paperdoll.core.layer.Member
import paperdoll.core.layer.Layers
import shapeless.Coproduct
import scalaz.Leibniz.===
import paperdoll.core.effect.Pure
import scalaz.Forall
import paperdoll.core.effect.Arr_
import paperdoll.core.effect.Arrs
import paperdoll.core.effect.Impure
import paperdoll.core.queue.Queue
import paperdoll.core.effect.Bind
import paperdoll.core.effect.Arr
import paperdoll.core.effect.Handler
import shapeless.Nat

sealed trait Region[S <: Nat, R, A] {
  def fold[B](resource: (A === R, R) => B): B
}

object Region {
  /**
   * S is an index to allow us to have multiple regions for resources
   * of the same type. Use a different S for each resource in the same
   * effect stack (e.g. when opening two or more files).
   * (Reusing the same S for different *types* of resource R should
   * work but I still don't recommend it)
   * If writing a library method that returns an effect stack that
   * includes regions, best practice is to take an "offset" argument
   * and start numbering from there, so that client code that calls
   * two or more such library methods can ensure the types don't overlap.
   */
  def newSHandle[S <: Nat, R](s: S, r: => R): Effects.One[Region_[S, R], R] =
    Effects.send[Region_[S, R], R](new Region[S, R, R] {
      override def fold[B](resource: (R === R, R) => B) =
        resource(Leibniz.refl, r)
    })

  private[this] def handleInRgn[S <: Nat, RE](handle: RE)(implicit re: Resource[RE]) = Effects.handle(new Bind[Region_[S, RE]] {
    override type O[X] = X
    override def pure[A](a: A) = {
      re.close(handle)
      a
    }
    override def apply[V, RR <: Coproduct, RL <: Layers[RR], A](eff: Region[S, RE, V], cont: Arr[RR, RL, V, O[A]]) =
      throw new RuntimeException("Opened the same handle twice. Did you reuse the same S type for multiple regions?")
  })

  def newRgn[S <: Nat, RE](implicit re: Resource[RE]): Handler[Region_[S, RE]] = new Handler[Region_[S, RE]] {
    override type O[X] = X
    override def run[R <: Coproduct, L1 <: Layers[R], A](eff: Effects[R, L1, A])(
      implicit me: Member[R, Region_[S, RE]] { type L = L1 }): Effects[me.RestR, me.RestL, O[A]] =
      eff.fold(
        a => Pure[me.RestR, me.RestL, A](a),
        new Forall[({ type K[X] = (me.L#O[X], Arrs[R, me.L, X, A]) ⇒ Effects[me.RestR, me.RestL, O[A]] })#K] {
          override def apply[X] = { (eff, cont) ⇒
            val composed = Effects.compose(cont)
            me.remove(eff).fold(
              otherEffect ⇒ Impure[me.RestR, me.RestL, X, O[A]](otherEffect, Queue.one[Arr_[me.RestR, me.RestL]#O, X, O[A]](
                composed andThen { run(_) })),
              _.fold({ (le, r) =>
                re.open(r)
                handleInRgn[S, RE](r).apply(le.subst[({ type K[Y] = Arr[R, L1, Y, A] })#K](composed)(r))
              }))
          }
        })
  }
}