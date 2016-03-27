package paperdoll

import paperdoll.core.layer.Layer

package object arm {
  type Region_[S, R] = Layer {
    type F[X] = Region[S, R, X]
  }
}