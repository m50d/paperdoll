package paperdoll.scalaz

import paperdoll.core.effect.Effects
import paperdoll.core.layer.Layers
import shapeless.{:+:, CNil}

object ReaderWriterStateLayer {
  def sendReaderWriterState[I, W, S, A]: Effects[
    Reader_[I] :+: Writer_[W] :+: State_[S] :+: CNil,
    Layers[Reader_[I] :+: Writer_[W] :+: State_[S] :+: CNil] {
      type O[X] = Reader_[I]#F[X] :+: Writer_[W]#F[X] :+: State_[S]#F[X] :+: CNil
    }, A] = ???
}