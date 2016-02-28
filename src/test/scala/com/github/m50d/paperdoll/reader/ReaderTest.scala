package com.github.m50d.paperdoll.reader

import org.junit.Test
import scalaz.syntax.monad._
import Reader._
import com.github.m50d.paperdoll.effect.Eff
import shapeless.{ CNil, :+: }
import org.fest.assertions.Assertions.assertThat
import com.github.m50d.paperdoll.layer.Layers
import com.github.m50d.paperdoll.layer.Member
import scalaz.Leibniz

class ReaderTest {
  @Test def basicFunctionality(): Unit = {
    val reader =
      for {
        fst <- askReaderOnly[Int]
        snd <- askReaderOnly[Int]
      } yield fst + snd

    val lr = Layers[Reader_[Int] :+: CNil]
    val pure = runReader[Int, Reader_[Int] :+: CNil, Layers.Aux[Reader_[Int] :+: CNil, lr.O], Int, Layers.Aux[Reader_[Int] :+: CNil, lr.O]](4, reader)(
        Member.nil[Reader_[Int], CNil], Leibniz.refl)
    val result = Eff.run(pure)
    assertThat(result).isEqualTo(8)
  }
}