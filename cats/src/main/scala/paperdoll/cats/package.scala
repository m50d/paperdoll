package paperdoll

import paperdoll.core.layer.Layer
import java.io.Writer
import _root_.cats.data.Xor


package object cats {
    type Xor_[A] = Layer {
    type F[B] = Xor[A, B]
  }
}