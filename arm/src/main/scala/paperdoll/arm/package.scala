package paperdoll

import paperdoll.core.layer.Layer

package object arm {
  type SHandle_[S] = Layer {
    type F[X] = SHandle[S, X]
  }
}