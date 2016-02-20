package com.github.m50d.paperdoll.reader

import org.junit.Test
import scalaz.syntax.monad._
import Reader._
import com.github.m50d.paperdoll.Eff
import shapeless.CNil
import org.fest.assertions.Assertions.assertThat

class ReaderTest {
  @Test def basicFunctionality(): Unit = {
    val reader =
      for {
        fst <- askReaderOnly[Int]
        snd <- askReaderOnly[Int]
      } yield fst + snd
      
    val pure = runReader[Int, CNil, ({type L[X] = CNil})#L, Int](4, reader)
    val result = Eff.run(pure)
    assertThat(result).isEqualTo(8)
  }
}