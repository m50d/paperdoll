package paperdoll.scalaz

import scalaz.Functor
import scalaz.OptionT
import paperdoll.core.effect.Effects
import scalaz.std.option._
import paperdoll.core.layer.Layer
import paperdoll.std.Option_

object OptionTLayer {
  def sendOptionT[F[_]: Functor, A](ot: OptionT[F, A]): Effects.Two[Layer.Aux[F], Option_, A] =
    Effects.sendTU[F[Option[A]], Option[A]](ot.run)
}