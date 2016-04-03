package paperdoll.std

import paperdoll.core.effect.Bind
import paperdoll.core.effect.Handler
import paperdoll.core.effect.Eff
import paperdoll.core.effect.Arr
import paperdoll.core.effect.Pure
import paperdoll.core.layer.Layers
import shapeless.Coproduct
import scalaz.syntax.monad._

object OptionLayer {
  def runOption: Handler.Aux[Option_, Option] =
    Eff.handle(new Bind[Option_] {
      override type O[X] = Option[X]
      override def pure[A](a: A) = Some(a)
      override def apply[V, RR <: Coproduct, RL <: Layers[RR], A](eff: Option[V], cont: Arr[RR, RL, V, Option[A]]) =
        eff.fold[Eff[RR, RL, Option[A]]](Pure(None))(cont)
    })
}