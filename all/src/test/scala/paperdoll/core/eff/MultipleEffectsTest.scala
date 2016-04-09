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
    
    val expected = (100, Vector("begin", "end"))
    val _1 = assertThat(handleReader(10)(handleWriterCollection[String, Vector[String]].apply(rdwr)).run).isEqualTo(expected)
    val _2 = assertThat(handleWriterCollection[String, Vector[String]].apply(handleReader(10)(rdwr)).run).isEqualTo(expected)
  }
}