package com.github.m50d.paperdoll

import org.junit.Test
import scalaz.syntax.functor._
import scalaz.syntax.monad._
import scalaz.syntax.bind._

class ReaderTest {

  @Test def basicFunctionality(): Unit = {
    val reader =
      for {
        fst <- example.askReaderOnly[Int]
        snd <- example.askReaderOnly[Int]
      } yield fst + snd
  }
}