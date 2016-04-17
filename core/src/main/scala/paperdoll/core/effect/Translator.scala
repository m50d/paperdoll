package paperdoll.core.effect

import scalaz.Leibniz
import paperdoll.core.layer.Member
import shapeless.Coproduct
import paperdoll.core.layer.Layer
import paperdoll.core.layer.Layers
import paperdoll.core.layer.Subset

trait Translator[L <: Layer] {
  type OR <: Coproduct
  type OL <: Layers[OR]
  /**
   * Actual implementation - apply method has extra Leibniz to assist type inference.
   */
  def run[R <: Coproduct, L1 <: Layers[R], RR <: Coproduct, RL <: Layers[RR], A](eff: Effects[R, L1, A])(
      implicit me: Member[R, L] {
        type L = L1
        type RestR = RR
        type RestL = RL
        },
      su: Subset[RR, OR] {
          type LT = OL
          type LS = RL
        }      
  ): Effects[me.RestR, me.RestL, A]
  final def apply[R <: Coproduct, L1 <: Layers[R], A, L2 <: Layers[R], RR <: Coproduct, RL <: Layers[RR]](
      eff: Effects[R, L1, A])(implicit me: Member[R, L] { 
        type L = L2
        type RestR = RR
        type RestL = RL
      },
    le: Leibniz[Nothing, Layers[R], L1, L2],
    su: Subset[RR, OR]{
        type LS = RL
        type LT = OL
      }): Effects[me.RestR, me.RestL, A] =
    run(le.subst[({ type L[X <: Layers[R]] = Effects[R, X, A] })#L](eff))(me, su)
}
object Translator {
  type Aux[L <: Layer, OR0 <: Coproduct] = Translator[L] {
    type OR = OR0
  }
}