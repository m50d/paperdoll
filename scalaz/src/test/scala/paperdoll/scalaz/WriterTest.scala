package paperdoll.scalaz

import org.junit.Test
import scalaz.syntax.monad._
import scalaz.std.anyVal._
import WriterLayer._
import org.fest.assertions.Assertions.assertThat

class WriterTest {
  @Test def basicFunctionality(): Unit = {
    val effect = for {
      _ <- sendTell(1)
      _ <- sendTell(3)
      _ <- sendTell(2)
    } yield "WriterTest"
    
    val _1 = assertThat(runWriterVector[Int](effect).run).isEqualTo(("WriterTest", Vector(1, 3, 2)))
    val _2 = assertThat(runWriterMonoid[Int].apply(effect).run).isEqualTo(("WriterTest", 6))
  }
}