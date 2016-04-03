package paperdoll

import paperdoll.core.layer.Layer

package object std {
  type Option_ = Layer {
    type F[X] = Option[X]
  }
  type Either_[A] = Layer {
    type F[X] = Either[A, X]
  }
}