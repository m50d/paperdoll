package paperdoll.scalaz.concurrent

import paperdoll.core.effect.Effects.sendU
import scalaz.concurrent.Future
import paperdoll.core.effect.Effects
import paperdoll.core.layer.Layer

object FutureLayer {
  def sendFuture[A](fa: Future[A]): Effects.One[Future_, A] = sendU(fa)
}