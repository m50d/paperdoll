package paperdoll.core.effect

import paperdoll.core.layer.{ Layer, Layers }
import shapeless.Coproduct
import paperdoll.core.layer.Member
import scalaz.Forall
import paperdoll.core.queue.Queue
import Arrs.compose

/** The simplest/most common implementation of Handler,
 *  supplying implementations for pure and bind
 */
trait Bind[L <: Layer] extends Handler[L] {
  /** How the type of the value is transformed by handling this effect
   */
  type O[X]

  /** Translate the pure value a
   */
  def pure[A](a: A): O[A]
  /** Translate the effectful value eff (with an effect from layer L)
   *  and effectful continuation cont (V => A with effects R)
   *  into an effectful (lazy) value of type A
   *  (usually by somehow "running" eff to obtain a V
   *  and then passing it to cont)
   */
  def bind[V, RR <: Coproduct, RL <: Layers[RR], A](eff: L#F[V], cont: Arr[RR, RL, V, O[A]]): Effects[RR, RL, O[A]]

  final override def run[R <: Coproduct, L1 <: Layers[R], A](eff: Effects[R, L1, A])(implicit me: Member[R, L] { type L = L1 }): Effects[me.RestR, me.RestL, O[A]] =
    eff.fold({ a ⇒ Pure[me.RestR, me.RestL, O[A]](pure(a)) }, new Forall[({ type K[X] = (me.L#O[X], Arrs[R, me.L, X, A]) ⇒ Effects[me.RestR, me.RestL, O[A]] })#K] {
      override def apply[X] = { (eff, cont) ⇒
        //New continuation is: recursively run this handler on the result of the old continuation 
        val newCont = compose(cont) andThen { run(_) }
        me.remove(eff).fold(
          otherEffect ⇒ Impure[me.RestR, me.RestL, X, O[A]](otherEffect, Queue.one[Arr_[me.RestR, me.RestL]#O, X, O[A]](newCont)),
          thisEffect ⇒ bind[X, me.RestR, me.RestL, A](thisEffect, newCont))
      }
    })
}