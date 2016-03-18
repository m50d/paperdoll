package paperdoll.core.queue

import org.junit.Test
import Predef.identity

class QueueTest {
  @Test def longQueue(): Unit = {
    val _ = Queue.empty[Function1, Int] :+ identity :+ identity :+ identity :+ identity :+ identity :+ identity
  }
}