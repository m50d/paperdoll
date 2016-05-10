package paperdoll.core.effect

import paperdoll.core.layer.Layers
import paperdoll.core.layer.Member
import paperdoll.core.layer.Layer
import scalaz.Forall
import shapeless.Coproduct
import paperdoll.core.queue.Queue
import Arrs.compose

/** Implementation helper for a common pattern in handling Effect layers.
 *  This is still quite low-level - most common use cases are covered by
 *  Handler and/or Translator.
 */
abstract class Loop[R <: Coproduct, L1 <: Layers[R], L <: Layer](implicit val me: Member[R, L] { type L = L1 }) {
  /**
   * "Output" type if we want to handle a layer by somehow "translating" the value A.
   * E.g. we can translate an Option_ effect into a stack without that effect, but
   * where the value "inside" is an Option.
   * We really want to allow arbitrary type-level functions here, but the overhead of
   * doing that in Scala doesn't seem worthwhile without convincing use cases.
   */
  type O[X]

  /**
   * Wrap up a pure value as an O[A]. Since any effectful stack might actually just
   * be a pure value a, if we're translating into O[A] we need to be able to translate pure values
   */
  def pure[A](a: A): O[A]
  
  /**
   * Handle a single effect eff and a continuation that's already been handled.
   * This is the "meat" (and the reason Loop has to be different for each possible L type).
   */
  def bind[V, A](eff: L#F[V], cont: Arr[me.RestR, me.RestL, V, O[A]]): Effects[me.RestR, me.RestL, O[A]]

  /**
   * Loop through eff handling the layer L, returning an O[A] in a smaller effect stack.
   */
  final def run[A](eff: Effects[R, L1, A]): Effects[me.RestR, me.RestL, O[A]] =
    eff.fold({ a ⇒ Pure[me.RestR, me.RestL, O[A]](pure(a)) },
      new Forall[({ type K[X] = (me.L#O[X], Arrs[R, me.L, X, A]) ⇒ Effects[me.RestR, me.RestL, O[A]] })#K] {
        override def apply[X] = { (eff, cont) ⇒
          //New continuation is: recursively run this handler on the result of the old continuation 
          val newCont = compose(cont) andThen { run(_) }
          me.remove(eff).fold(
            otherEffect ⇒ Impure[me.RestR, me.RestL, X, O[A]](otherEffect, Queue.one[Arr_[me.RestR, me.RestL]#O, X, O[A]](newCont)),
            thisEffect ⇒ bind[X, A](thisEffect, newCont))
        }
      })
}