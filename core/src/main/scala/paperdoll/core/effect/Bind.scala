package paperdoll.core.effect

import paperdoll.core.layer.{ Layer, Layers }
import shapeless.Coproduct
import paperdoll.core.layer.Member
import scalaz.Forall
import paperdoll.core.effect.Arrs.compose
import paperdoll.queue.Queue

/** TODO update
 *  Handle an effect layer that works by recursively handling subsequent effects,
 *  and having some method to compose an effect with a continuation in which
 *  	subsequent cases of that effect have been handled.
 *  Purely an implementation helper - anything done with this could be done
 *  by calling Effects#fold directly.
 *  This is still quite low-level - most common use cases are covered by
 *  GenericBind and/or GenericTranslator.
 */
trait Bind[R <: Coproduct, L1 <: Layers[R], L <: Layer] extends Handler[R, L1, L] {

  /** Wrap up a pure value as an O[A]. Since any effectful stack might actually just
   *  be a pure value a, if we're translating into O[A] we need to be able to translate pure values
   */
  def pure[A](a: A): O[A]

  /** Handle a single effect eff and a continuation that's already been handled.
   *  This is the "meat" (and the reason Loop has to be different for each possible L type).
   */
  def bind[V, A](eff: L#F[V], cont: Arr[RestR, RestL, V, O[A]]): Effects[RestR, RestL, O[A]]

  /** Loop through eff handling the layer L, returning an O[A] in a smaller effect stack.
   */
  final def run[A](eff: Effects[R, L1, A]): Effects[RestR, RestL, O[A]] =
    eff.fold({ a ⇒ Pure[RestR, RestL, O[A]](pure(a)) },
      new Forall[({ type K[X] = (L1#O[X], Arrs[R, L1, X, A]) ⇒ Effects[RestR, RestL, O[A]] })#K] {
        override def apply[X] = { (eff, cont) ⇒
          //New continuation is: recursively run this handler on the result of the old continuation 
          val newCont = compose(cont) andThen { run(_) }
          me.remove(eff).fold(
            otherEffect ⇒ Impure[RestR, RestL, X, O[A]](otherEffect, Queue.one[Arr_[RestR, RestL]#O, X, O[A]](newCont)),
            thisEffect ⇒ bind[X, A](thisEffect, newCont))
        }
      })
}

trait GenericBind[L <: Layer] extends GenericHandler[L] {
  def pure[A](a: A): O[A]
  def bind[V, RR <: Coproduct, RL <: Layers[RR], A](eff: L#F[V], cont: Arr[RR, RL, V, O[A]]): Effects[RR, RL, O[A]]

  override def handler[R <: Coproduct](implicit me1: Member[R, L]): Handler[R, me1.L, L] {
    type RestR = me1.RestR
    type RestL = me1.RestL
    type O[X] = GenericBind.this.O[X]
  } =
    new Bind[R, me1.L, L] {
      type RestR = me1.RestR
      type RestL = me1.RestL
      override def me = me1
      override def pure[A](a: A) = GenericBind.this.pure(a)
      override type O[X] = GenericBind.this.O[X]
      override def bind[V, A](eff: L#F[V], cont: Arr[RestR, RestL, V, O[A]]) = GenericBind.this.bind(eff, cont)
    }
}