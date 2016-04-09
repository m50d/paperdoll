package paperdoll.scalaz

import scalaz.Functor
import scalaz.OptionT
import paperdoll.core.effect.Effects
import scalaz.std.option._

object OptionTLayer {
  def sendOptionT[F[_]: Functor, A](ot: OptionT[F, A]) = Effects.sendTU(ot.run)
}