package com.github.m50d.paperdoll

import org.junit.Test
import scalaz.syntax.monad._
import shapeless.{CNil, :+:, Coproduct}
import example.Reader_
import scalaz.Unapply
import scalaz.Leibniz
import org.fest.assertions.Assertions.assertThat

class ReaderTest {
  @Test def basicFunctionality(): Unit = {
    val reader =
      for {
        fst <- example.askReaderOnly[Int]
        snd <- example.askReaderOnly[Int]
      } yield fst + snd
      
    val pure = example.runReader[Int, CNil, Int, ({type L[X] = CNil})#L](4, reader)
    val result = Eff.run(pure)
    assertThat(result).isEqualTo(8)
  }
}