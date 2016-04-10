package paperdoll.cats

import paperdoll.core.effect.Effects
import paperdoll.core.layer.Layer
import paperdoll.std.Option_
import CatsEffects.sendTUC
import cats.Functor
import cats.data.OptionT
import cats.std.option._

object OptionTLayer {
  def sendOptionT[F[_]: Functor, A](ot: OptionT[F, A]): Effects.Two[Layer.Aux[F], Option_, A] =
    sendTUC[F[Option[A]], Option[A]](ot.value)
}