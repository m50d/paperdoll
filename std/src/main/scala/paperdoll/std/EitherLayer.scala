package paperdoll.std

import paperdoll.core.effect.GenericBind
import paperdoll.core.effect.GenericHandler
import paperdoll.core.effect.Effects
import paperdoll.core.effect.Effects.sendU
import paperdoll.core.effect.Arr
import paperdoll.core.effect.Pure
import paperdoll.core.layer.Layers
import shapeless.Coproduct
import scalaz.std.either._

object EitherLayer {
  def sendEither[A, B](either: Either[A, B]): Effects.One[Either_[A], B] =
    sendU(either)

  /** Either is handled much like Option: if Right,
   *  run the continuation, if Left, return that.
   */
  def handleEither[A]: GenericHandler.Aux[Either_[A], Either_[A]#F] =
    new GenericBind[Either_[A]] {
      override type O[X] = Either[A, X]
      override def pure[B](b: B) = Right(b)
      override def bind[V, RR <: Coproduct, RL <: Layers[RR], B](eff: Either[A, V], cont: Arr[RR, RL, V, Either[A, B]]) =
        eff.fold(l ⇒ Pure(Left(l)), cont)
    }
}