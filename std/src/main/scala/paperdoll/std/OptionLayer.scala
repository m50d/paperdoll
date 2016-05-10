package paperdoll.std

import paperdoll.core.effect.PureBind
import paperdoll.core.effect.PureHandler
import paperdoll.core.effect.Effects
import paperdoll.core.effect.Arr
import paperdoll.core.effect.Pure
import paperdoll.core.layer.Layers
import shapeless.Coproduct

object OptionLayer {
  /** Options are handled by: if Some, run the continuation, otherwise
   *  return Pure(None)
   */
  def handleOption: PureHandler.Aux[Option_, Option] =
    new PureBind[Option_] {
      override type O[X] = Option[X]
      override def pure[A](a: A) = Some(a)
      override def bind[V, RR <: Coproduct, RL <: Layers[RR], A](eff: Option[V], cont: Arr[RR, RL, V, Option[A]]) =
        eff.fold[Effects[RR, RL, Option[A]]](Pure(None))(cont)
    }
}