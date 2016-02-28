package com.github.m50d.paperdoll.reader

import org.junit.Test
import scalaz.syntax.monad._
import Reader._
import com.github.m50d.paperdoll.effect.Eff
import shapeless.{ CNil, :+: }
import org.fest.assertions.Assertions.assertThat
import com.github.m50d.paperdoll.layer.Layers

class ReaderTest {
  @Test def basicFunctionality(): Unit = {
    val reader =
      for {
        fst <- askReaderOnly[Int]
        snd <- askReaderOnly[Int]
      } yield fst + snd

    val lr = Layers[Reader_[Int] :+: CNil]
    val l0 = Layers[CNil]
    val pure = runReader[Int, Reader_[Int] :+: CNil, Layers.Aux[Reader_[Int] :+: CNil, lr.O], CNil, Layers.Aux[CNil, l0.O], Layers.Aux[Reader_[Int] :+: CNil, lr.O], Int](4, reader)
    val result = Eff.run(pure)
    assertThat(result).isEqualTo(8)
  }
}