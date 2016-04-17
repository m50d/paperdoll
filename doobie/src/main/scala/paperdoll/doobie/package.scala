package paperdoll

import paperdoll.core.layer.Layer
import _root_.doobie.imports.ConnectionIO

package object doobie {
  type ConnectionIO_ = Layer {
    type F[X] = ConnectionIO[X]
  }
}