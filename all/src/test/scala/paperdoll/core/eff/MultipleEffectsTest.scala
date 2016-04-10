package paperdoll.core.eff

import org.junit.Test
import scalaz.syntax.monad._
import paperdoll.scalaz.ReaderLayer._
import paperdoll.scalaz.WriterLayer._
import shapeless.{:+:, CNil}
import paperdoll.scalaz.Writer_
import paperdoll.scalaz.Reader_
import org.fest.assertions.Assertions.assertThat

class MultipleEffectsTest {
  /**
   * Example functions from the paper
   */
  def addGet(x: Int) = for {
    i <- sendAsk[Int]
  } yield i+x
  def addN(n: Int) = sendAsk[Int].replicateM(n).map(_.sum)
  
  @Test def multipleEffects(): Unit = {
    type ReaderWriter = Reader_[Int] :+: Writer_[String] :+: CNil
    val rdwr = for {
      _ <- sendTell("begin").extend[ReaderWriter]()
      r <- addN(10).extend[ReaderWriter]()
      _ <- sendTell("end").extend[ReaderWriter]()
    } yield r
    
    val readerHandler = handleReader(10)
    val writerHandler = handleWriterCollection[String, Vector[String]]
    val expected = (Vector("begin", "end"), 100)
    val _1 = assertThat(readerHandler(writerHandler(rdwr)).run).isEqualTo(expected)
    val _2 = assertThat(writerHandler(readerHandler(rdwr)).run).isEqualTo(expected)
  }
}