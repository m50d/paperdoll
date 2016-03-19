package paperdoll.writer

import org.junit.Test
import org.fest.assertions.Assertions.assertThat
import scalaz.syntax.monad._
import scalaz.std.anyVal._
import Writer._
import paperdoll.core.effect.Eff

class WriterTest {
  @Test def basicFunctionality(): Unit = {
    val effect = for {
      _ <- tell(1)
      _ <- tell(3)
      _ <- tell(2)
    } yield "WriterTest"
    
    val _1 = assertThat(runWriterVector[Int](effect).run).isEqualTo(("WriterTest", Vector(1, 3, 2)))
    val _2 = assertThat(runWriterMonoid[Int].apply(effect).run).isEqualTo(("WriterTest", 6))
  }
}