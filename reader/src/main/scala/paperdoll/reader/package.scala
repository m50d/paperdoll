package paperdoll

import paperdoll.core.layer.Layer

package object reader {
  type Reader_[I] = Layer {
    type F[X] = Reader[I, X]
  }
}