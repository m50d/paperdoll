package paperdoll

import paperdoll.core.layer.Layer
import shapeless.Nat

package object arm {
  type Region_[S <: Nat, R] = Layer {
    type F[X] = Region[S, R, X]
  }
}