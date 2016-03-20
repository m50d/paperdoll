package paperdoll

import paperdoll.core.layer.Layer

package object nondeterminism {
  type NDet_ = Layer {
    type F[X] = NDet[X]
  }
}