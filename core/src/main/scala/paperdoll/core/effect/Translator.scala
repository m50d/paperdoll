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
  /** Actual implementation - apply method has extra Leibniz to assist type inference.
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
    }): Effects[me.RestR, me.RestL, A]
  final def apply[R <: Coproduct, L1 <: Layers[R], A, L2 <: Layers[R], RR <: Coproduct, RL <: Layers[RR], LT0 <: Layers[OR]](
    eff: Effects[R, L1, A])(implicit me: Member[R, L] {
      type L = L2
      type RestR = RR
      type RestL = RL
    },
      su: Subset[RR, OR] {
        type LS = RL
        type LT = LT0
      },
      //FIXME: inconsistent in which way I substitute, should resolve this generally
      le1: Leibniz[Nothing, Layers[R], L1, L2],
      le2: Leibniz[Nothing, Layers[OR], LT0, OL]): Effects[me.RestR, me.RestL, A] =
    run(le1.subst[({ type K[X <: Layers[R]] = Effects[R, X, A] })#K](eff))(me, le2.subst[({
      type K[X <: Layers[OR]] = Subset[RR, OR] {
        type LS = RL
        type LT = X
      }
    })#K](su))
}
object Translator {
  type Aux[L <: Layer, OR0 <: Coproduct, OL0 <: Layers[OR0]] = Translator[L] {
    type OR = OR0
    type OL = OL0
  }
}