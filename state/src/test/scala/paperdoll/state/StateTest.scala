package paperdoll.state

import org.junit.Test
import paperdoll.reader.Reader
import shapeless.{:+:, CNil}
import scalaz.syntax.monad._
import paperdoll.writer.Writer_
import paperdoll.reader.Reader_

class StateTest {
  @Test def basicFunctionality(): Unit = {
    type ReaderWriter = Reader_[Int] :+: Writer_[Int] :+: CNil
    val eff = for {
      firstValue <- Reader.ask[Int].extend[ReaderWriter]()
    } yield {}
  }

}