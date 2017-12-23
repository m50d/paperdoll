package paperdoll.queue

import org.junit.Test
import scala.Predef.identity

class QueueTest {
  @Test def longQueue(): Unit = {
    val _ = Queue.Empty[Function1, Int] :+ identity :+ identity :+ identity :+ identity :+ identity :+ identity
  }
}