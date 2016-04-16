package paperdoll.scalaz

import paperdoll.core.layer.Layer
import scalaz.concurrent.Future

package object concurrent {
  type Future_ = Layer {
    type F[X] = Future[X]
  }
}