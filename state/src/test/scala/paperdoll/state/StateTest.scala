package paperdoll.state

import org.junit.Test
import paperdoll.scalaz.Reader
import shapeless.{:+:, CNil}
import scalaz.syntax.monad._
import paperdoll.scalaz.Writer_
import paperdoll.scalaz.Reader_
import paperdoll.scalaz.WriterLayer
import org.fest.assertions.Assertions.assertThat

class StateTest {
  @Test def basicFunctionality(): Unit = {
    type ReaderWriter = Reader_[Int] :+: Writer_[Int] :+: CNil
    val eff = for {
      firstValue <- Reader.ask[Int].extend[ReaderWriter]()
      _ <- WriterLayer.sendTell(firstValue * 2).extend[ReaderWriter]()
      secondValue <- Reader.ask[Int].extend[ReaderWriter]()
    } yield (firstValue, secondValue)
    val _ = assertThat(State.runState[Int](eff, 4).run).isEqualTo(((4, 8), 8))
  }

}