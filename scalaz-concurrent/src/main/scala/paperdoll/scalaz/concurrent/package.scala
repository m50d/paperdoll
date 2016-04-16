package paperdoll.scalaz

import paperdoll.core.layer.Layer
import scalaz.concurrent.Future
import scalaz.concurrent.Task

package object concurrent {
  type Future_ = Layer {
    type F[X] = Future[X]
  }
  type Task_ = Layer {
    type F[X] = Task[X]
  }
}