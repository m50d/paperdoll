package paperdoll.core.effect

import scalaz.Leibniz
import paperdoll.core.layer.Member
import shapeless.Coproduct
import paperdoll.core.layer.Layer
import paperdoll.core.layer.Layers
import paperdoll.core.layer.Subset

trait GenericTranslator[L <: Layer] {
  type OR <: Coproduct
  type OL <: Layers[OR]
  def handler[R <: Coproduct, L1 <: Layers[R], RR <: Coproduct, RL <: Layers[RR]](
    implicit me: Member[R, L] {
      type L = L1
      type RestR = RR
      type RestL = RL
    },
    su: Subset[RR, OR] {
      type LT = OL
      type LS = RL
    }): Handler[R, L1, L] {
    type RestR = RR
    type RestL = RL
    type O[X] = X
  }
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
      le1: Leibniz[Nothing, Layers[R], L2, L1],
      le2: Leibniz[Nothing, Layers[OR], LT0, OL]): Effects[RR, RL, A] =
    handler[R, L1, RR, RL](le1.subst[
      ({type K[X] = Member[R, L] {
        type L = X
        type RestR = RR
        type RestL = RL
      }})#K  
    ](me), le2.subst[({
      type K[X <: Layers[OR]] = Subset[RR, OR] {
        type LS = RL
        type LT = X
      }
    })#K](su)).run(eff)
}
object GenericTranslator {
  type Aux[L <: Layer, OR0 <: Coproduct, OL0 <: Layers[OR0]] = GenericTranslator[L] {
    type OR = OR0
    type OL = OL0
  }
}