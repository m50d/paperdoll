package paperdoll

import paperdoll.core.layer.Layer
import _root_.scalaz.Disjunction
import _root_.scalaz.Writer

package object scalaz {
  type Disjunction_[A] = Layer {
    type F[B] = Disjunction[A, B]
  }
  type Writer_[O] = Layer {
    type F[X] = Writer[O, X]
  }
  type Reader_[I] = Layer {
    type F[X] = Reader[I, X]
  }
}