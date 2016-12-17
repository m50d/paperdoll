package paperdoll.cats

import paperdoll.core.effect.Effects
import paperdoll.core.layer.Layer
import CatsEffects.sendTUC
import cats.Functor
import cats.data.EitherT
import paperdoll.std.Either_
import cats.instances.either._

object EitherTLayer {
  def sendEitherT[F[_]: Functor, A, B](et: EitherT[F, A, B]): Effects.Two[Layer.Aux[F], Either_[A], B] =
    sendTUC[F[Either[A, B]], Either[A, B]](et.value)
}