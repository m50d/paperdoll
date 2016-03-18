package paperdoll

import paperdoll.core.layer.Layer

package object writer {
  type Writer_[O] = Layer {
    type F[X] = Writer[O, X]
  }
}