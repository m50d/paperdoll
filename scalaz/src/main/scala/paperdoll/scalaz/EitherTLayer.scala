package paperdoll.scalaz

import scalaz.EitherT
import paperdoll.core.effect.Effects
import scalaz.Functor

object EitherTLayer {
  def sendEitherT[F[_]: Functor, A, B](et: EitherT[F, A, B]) = Effects.sendTU(et.run)
}