package paperdoll.cats

import cats.data.Xor
import paperdoll.core.effect.Arr
import paperdoll.core.effect.PureHandler
import shapeless.Coproduct
import paperdoll.core.layer.Layers
import paperdoll.core.effect.PureBind
import paperdoll.core.effect.Effects
import paperdoll.core.effect.Pure
import CatsEffects.sendUC

object XorLayer {
    def sendXor[A, B](xor: Xor[A, B]): Effects.One[Xor_[A], B] =
    sendUC(xor)
    /** Xor is handled much like Option: if \/-,
   *  run the continuation, if -\/, return that.
   */
  def handleXor[A]: PureHandler.Aux[Xor_[A], Xor_[A]#F] =
    new PureBind[Xor_[A]] {
      override type O[X] = Xor[A, X]
      override def pure[B](b: B) = Xor.Right(b)
      override def bind[V, RR <: Coproduct, RL <: Layers[RR], B](
          eff: Xor[A, V], cont: Arr[RR, RL, V, Xor[A, B]]) =
        eff.fold(l ⇒ Pure(Xor.Left(l)), cont)
    }
}