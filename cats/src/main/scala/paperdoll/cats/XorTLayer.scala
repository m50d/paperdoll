package paperdoll.cats

import paperdoll.core.effect.Effects
import paperdoll.core.layer.Layer
import CatsEffects.sendTUC
import cats.Functor
import cats.data.{Xor, XorT}

object XorTLayer {
  def sendXorT[F[_]: Functor, A, B](xot: XorT[F, A, B]): Effects.Two[Layer.Aux[F], Xor_[A], B] =
    sendTUC[F[Xor[A, B]], Xor[A, B]](xot.value)
}