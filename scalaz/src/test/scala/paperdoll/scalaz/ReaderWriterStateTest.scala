package paperdoll.scalaz

import org.junit.Test
import ReaderWriterStateLayer.sendReaderWriterState
import scalaz.ReaderWriterState
import ReaderLayer.handleReader
import WriterLayer.handleWriterMonoid
import StateLayer.handleState
import scalaz.std.anyVal._
import org.fest.assertions.Assertions._

class ReaderWriterStateTest {
  @Test def basicFunctionality(): Unit = {
    val effect = sendReaderWriterState(ReaderWriterState({
      (count: Int, state: String) =>
        (count * 2, "meh", state + count)
    }))
    val stateHandler = handleState("prefix")
    val readerHandler = handleReader(4)
    val writerHandler = handleWriterMonoid[Int]
    
    val _1 = assertThat(writerHandler(readerHandler(stateHandler(effect))).run)
      .isEqualTo((8, ("prefix4", "meh")))
    val _2 = assertThat(readerHandler(stateHandler(writerHandler(effect))).run)
      .isEqualTo(("prefix4", (8, "meh")))
  }
}