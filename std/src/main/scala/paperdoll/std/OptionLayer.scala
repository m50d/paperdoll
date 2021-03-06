package paperdoll.std

import paperdoll.core.effect.GenericBind
import paperdoll.core.effect.GenericHandler
import paperdoll.core.effect.Effects
import paperdoll.core.effect.Arr
import paperdoll.core.effect.Pure
import paperdoll.core.layer.Layers
import shapeless.Coproduct

object OptionLayer {
  /** Options are handled by: if Some, run the continuation, otherwise
   *  return Pure(None)
   */
  def handleOption: GenericHandler.Aux[Option_, Option] =
    new GenericBind[Option_] {
      override type O[X] = Option[X]
      override def pure[A](a: A) = Some(a)
      override def bind[V, RR <: Coproduct, RL <: Layers[RR], A](eff: Option[V], cont: Arr[RR, RL, V, Option[A]]) =
        eff.fold[Effects[RR, RL, Option[A]]](Pure(None))(cont)
    }
}