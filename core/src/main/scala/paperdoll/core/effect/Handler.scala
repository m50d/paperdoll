package paperdoll.core.effect

import scalaz.Leibniz
import paperdoll.core.layer.Member
import shapeless.Coproduct
import paperdoll.core.layer.Layer
import paperdoll.core.layer.Layers

/**
 * Handler for the effect L: given an effectful value in a
 * stack of effects R containing L, return an effectful value
 * in the stack with L removed.
 * This is purely an implementation helper - it's possible to
 * handle a concrete effectful value directly (using Effects#fold)
 * but this interface makes the type inference easier
 * and supports the common use case of instantiating a handler
 * for a specific layer separately from applying it to a stack
 * of layers that contains that layer
 * TODO: unify with Translator as far as possible
 */
trait Handler[L <: Layer] {
  type O[X]
  /**
   * Actual implementation - apply method has extra Leibniz to assist type inference.
   */
  def run[R <: Coproduct, L1 <: Layers[R], A](eff: Effects[R, L1, A])(implicit me: Member[R, L] { type L = L1 }): Effects[me.RestR, me.RestL, O[A]]
  final def apply[R <: Coproduct, L1 <: Layers[R], A, L2 <: Layers[R]](eff: Effects[R, L1, A])(implicit me: Member[R, L] { type L = L2 },
    le: Leibniz[Nothing, Layers[R], L1, L2]): Effects[me.RestR, me.RestL, O[A]] =
    run(le.subst[({ type L[X <: Layers[R]] = Effects[R, X, A] })#L](eff))(me)
}
object Handler {
  type Aux[L <: Layer, O0[_]] = Handler[L] {
    type O[X] = O0[X]
  }
}