package paperdoll

import paperdoll.core.layer.Layer
import _root_.doobie.imports._

package object doobie {
  type ConnectionIO_ = Layer {
    type F[X] = ConnectionIO[X]
  }
  type PreparedStatementIO_ = Layer {
    type F[X] = PreparedStatementIO[X]
  }
  type StatementIO_ = Layer {
    type F[X] = StatementIO[X]
  }
  type ResultSetIO_ = Layer {
    type F[X] = ResultSetIO[X]
  }
}