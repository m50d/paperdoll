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
    
    val pure1 = runWriterVector[Int](effect)
    val _1 = assertThat(Eff.run(pure1)).isEqualTo(("WriterTest", Vector(1, 3, 2)))
    val pure2 = runWriterMonoid[Int].apply(effect)
    val _2 = assertThat(Eff.run(pure2)).isEqualTo(("WriterTest", 6))
  }
}