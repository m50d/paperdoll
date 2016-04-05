package paperdoll

import paperdoll.core.layer.Layer
import scala.util.Try

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
}