package paperdoll.scalaz

import scalaz.EitherT
import shapeless.{ :+:, CNil }
import paperdoll.core.effect.Effects
import paperdoll.core.layer.Layer
import paperdoll.core.layer.Layers
import scalaz.Disjunction
import scalaz.syntax.monad._
import scalaz.Functor

object EitherTLayer {
  def sendEitherT[F[_]: Functor, A, B](et: EitherT[F, A, B]) = Effects.sendTU(et.run)
}