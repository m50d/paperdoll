package paperdoll.core.eff

import org.junit.Test
import scalaz.syntax.monad._
import paperdoll.reader.Reader
import paperdoll.writer.Writer
import shapeless.{:+:, CNil}
import paperdoll.writer.Writer_
import paperdoll.reader.Reader_
import org.fest.assertions.Assertions.assertThat
import paperdoll.core.effect.Eff

class MultipleEffectsTest {
  /**
   * Example functions from the paper
   */
  def addGet(x: Int) = for {
    i <- Reader.ask[Int]
  } yield i+x
  def addN(n: Int) = Reader.ask[Int].replicateM(n).map(_.sum)
  
  @Test def multipleEffects(): Unit = {
    type ReaderWriter = Reader_[Int] :+: Writer_[String] :+: CNil
    val rdwr = for {
      _ <- Writer.tell("begin").extend[ReaderWriter]()
      r <- addN(10).extend[ReaderWriter]()
      _ <- Writer.tell("end").extend[ReaderWriter]()
    } yield r
    
    val pure1 = Reader.runReader(10)(Writer.runWriterVector[String].apply(rdwr)).run
  }
}