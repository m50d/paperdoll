package paperdoll

import com.github.m50d.paperdoll.layer.Layer
import paperdoll.reader.Reader

package object reader {
  type Reader_[I] = Layer {
    type F[X] = Reader[I, X]
  }
}