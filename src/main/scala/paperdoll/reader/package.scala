package paperdoll

import com.github.m50d.paperdoll.layer.Layer

package object reader {
  type Reader_[I] = Layer {
    type F[X] = Reader[I, X]
  }
}