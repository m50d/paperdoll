package paperdoll.queue

import org.junit.Test

class QueueTest {
  @Test def longQueue(): Unit = {
    val _ = Queue.empty[Function1, Int] :+ identity :+ identity :+ identity :+ identity :+ identity :+ identity
  }
}