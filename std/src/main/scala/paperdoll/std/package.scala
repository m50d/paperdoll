package paperdoll

import paperdoll.core.layer.Layer
import scala.util.Try
import scala.concurrent.Future

package object std {
  type Option_ = Layer {
    type F[X] = Option[X]
  }
  type Either_[A] = Layer {
    type F[X] = Either[A, X]
  }
  type Try_ = Layer {
    type F[X] = Try[X]
  }
  type Future_ = Layer {
    type F[X] = Future[X]
  }
}