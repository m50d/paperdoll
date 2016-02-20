package com.github.m50d.paperdoll

import org.junit.Test
import scalaz.syntax.monad._
import shapeless.{CNil}
import Reader._
import org.fest.assertions.Assertions.assertThat

class ReaderTest {
  @Test def basicFunctionality(): Unit = {
    val reader =
      for {
        fst <- askReaderOnly[Int]
        snd <- askReaderOnly[Int]
      } yield fst + snd
      
    val pure = runReader[Int, CNil, Int, ({type L[X] = CNil})#L](4, reader)
    val result = Eff.run(pure)
    assertThat(result).isEqualTo(8)
  }
}