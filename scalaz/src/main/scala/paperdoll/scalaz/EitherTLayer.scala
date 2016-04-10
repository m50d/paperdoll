package paperdoll.scalaz

import scalaz.EitherT
import paperdoll.core.effect.Effects
import scalaz.Functor
import paperdoll.core.layer.Layer
import scalaz.Disjunction

object EitherTLayer {
  def sendEitherT[F[_]: Functor, A, B](et: EitherT[F, A, B]): Effects.Two[Layer.Aux[F], Disjunction_[A], B] =
    Effects.sendTU[F[Disjunction[A, B]], Disjunction[A, B]](et.run)
}