package paperdoll.core.effect

import paperdoll.core.layer.Member
import paperdoll.core.layer.Layer
import paperdoll.core.layer.Layers
import shapeless.Coproduct
import scalaz.Leibniz

/** A class that can handle the effect represented by L in the stack R, L1.
 *  This is purely an implementation helper - any subclasses could also be
 *  written directly using Effects#fold.
 *  This is still quite low-level - most common use cases are covered by
 *  PureBind and/or PureTranslator
 */
trait Handler[R <: Coproduct, L1 <: Layers[R], L <: Layer] {
  type RestR <: Coproduct
  type RestL <: Layers[RestR]
  /** "Output" type if we want to handle a layer by changing the value A.
   *  E.g. we can translate an Option_ effect into a stack without that effect, but
   *  where the value "inside" is an Option.
   *  It is also common to have O=Id if we handle the effect by translating
   *  it into another effect in the stack, or if the effect is handled in a way
   *  that isn't exposed at the type level (e.g. via mutable state).
   *  Really O should be allowed to be an arbitrary type-level function of X,
   *  but there is no elegant way to encode that in Scala.
   */
  type O[X]
  
  def me: Member[R, L] {
    type L = L1
    type RestR = Handler.this.RestR
    type RestL = Handler.this.RestL
  }

  /** Handle the layer L in eff, returning a new Effects for a smaller stack
   *  (and possibly a transformed value).
   */
  def run[A](eff: Effects[R, L1, A]): Effects[RestR, RestL, O[A]]

}

trait PureHandler[L <: Layer] {
  type O[X]
  def handler[R <: Coproduct, L1 <: Layers[R]](implicit me1: Member[R, L] { type L = L1 }): Handler[R, L1, L] {
    type RestR = me1.RestR
    type RestL = me1.RestL
    type O[X] = PureHandler.this.O[X]
  }
  final def apply[R <: Coproduct, L1 <: Layers[R], A, L2 <: Layers[R]](eff: Effects[R, L1, A])(
    implicit me: Member[R, L] { type L = L2 },
    le: Leibniz[Nothing, Layers[R], L2, L1]): Effects[me.RestR, me.RestL, O[A]] =
    handler[R, L1](le.subst[({type K[X] = Member[R, L] {
      type L = X
      type RestR = me.RestR
      type RestL = me.RestL
    }})#K](me)).run(eff)
}
object PureHandler {
  type Aux[L <: Layer, O0[_]] = PureHandler[L] {
    type O[X] = O0[X]
  }
}