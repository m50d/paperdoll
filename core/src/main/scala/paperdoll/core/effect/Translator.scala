package paperdoll.core.effect

import scalaz.Leibniz
import paperdoll.core.layer.Member
import shapeless.{ CNil, :+:, Coproduct }
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
    handler[R, L1, RR, RL](le1.subst[({
      type K[X] = Member[R, L] {
        type L = X
        type RestR = RR
        type RestL = RL
      }
    })#K](me), le2.subst[({
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

/** TODO: Consider a variant of GenericTranslator that implements bind like this,
 *  but allows multiple layers in the OR/OL stack
 */
trait GenericSingleTranslator[L <: Layer] extends GenericTranslator[L] {
  type O <: Layer
  def handle[V](eff: L#F[V]): Effects[OR, OL, V]

  final override type OR = O :+: CNil
  final override type OL = Layers.One[O]

  final def handler[R <: Coproduct, L1 <: Layers[R], RR <: Coproduct, RL <: Layers[RR]](
    implicit me1: Member[R, L] { type L = L1; type RestR = RR; type RestL = RL },
    su: Subset[RR, OR] { type LT = OL; type LS = RL }): Handler[R, L1, L] { type RestR = RR; type RestL = RL; type O[X] = X } =
    new Bind[R, L1, L] {
      override type RestR = RR
      override type RestL = RL
      override type O[X] = X
      override def me = me1
      override def pure[A](a: A) = a
      override def bind[V, A](eff: L#F[V], cont: V ⇒ Effects[RR, RL, A]): Effects[RR, RL, A] =
        Effects.monadEffects.bind(handle(eff).extend[RR]())(cont)
    }
}