package paperdoll.scalaz

import paperdoll.core.effect.Effects
import paperdoll.core.layer.Layers
import shapeless.{ :+:, CNil }
import WriterLayer.sendWriter
import scalaz.{ Writer, State, Reader }
import StateLayer.sendState
import ReaderLayer.sendReader
import scalaz.ReaderWriterState
import scalaz.syntax.monad._

object ReaderWriterStateLayer {
  def sendReaderWriterState[I, W, S, A](rwsa: ReaderWriterState[I, W, S, A]): Effects[Reader_[I] :+: Writer_[W] :+: State_[S] :+: CNil, Layers[Reader_[I] :+: Writer_[W] :+: State_[S] :+: CNil] {
    type O[X] = Reader_[I]#F[X] :+: Writer_[W]#F[X] :+: State_[S]#F[X] :+: CNil
  }, A] = {
    type ReaderWriterStateR = Reader_[I] :+: Writer_[W] :+: State_[S] :+: CNil
    def sendWriterExtend(wa: (W, A)) = sendWriter(Writer(wa._1, wa._2)).extend[ReaderWriterStateR]()
    def sendStateExtend(wsa: S ⇒ (W, A, S)) = sendState(State({
      s: S ⇒
        val (w, a, newS) = wsa(s)
        (newS, (w, a))
    })).extend[ReaderWriterStateR]()
    def sendReaderExtend = sendReader(Reader({
      i: I ⇒
        { s: S ⇒ rwsa.run(i, s) }
    })).extend[ReaderWriterStateR]()
    for {
      wsa <- sendReaderExtend
      wa <- sendStateExtend(wsa)
      a <- sendWriterExtend(wa)
    } yield a
  }
}