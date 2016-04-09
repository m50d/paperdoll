package paperdoll.state

import org.junit.Test
import shapeless.{:+:, CNil}
import scalaz.syntax.monad._
import paperdoll.scalaz.Writer_
import paperdoll.scalaz.Reader_
import paperdoll.scalaz.WriterLayer._
import paperdoll.scalaz.ReaderLayer._
import org.fest.assertions.Assertions.assertThat
import scalaz.Reader

class StateTest {
  @Test def basicFunctionality(): Unit = {
    type ReaderWriter = Reader_[Int] :+: Writer_[Int] :+: CNil
    val eff = for {
      firstValue <- sendAsk[Int].extend[ReaderWriter]()
      _ <- sendTell(firstValue * 2).extend[ReaderWriter]()
      secondValue <- sendAsk[Int].extend[ReaderWriter]()
    } yield (firstValue, secondValue)
    val _ = assertThat(State.handleState[Int](eff, 4).run).isEqualTo(((4, 8), 8))
  }

}