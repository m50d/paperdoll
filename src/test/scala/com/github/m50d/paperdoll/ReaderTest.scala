package com.github.m50d.paperdoll

import org.junit.Test
import scalaz.syntax.monad._
import shapeless.{CNil, :+:, Coproduct}
import example.Reader_
import scalaz.Unapply
import scalaz.Leibniz

class ReaderTest {
  @Test def basicFunctionality(): Unit = {
    val reader =
      for {
        fst <- example.askReaderOnly[Int]
        snd <- example.askReaderOnly[Int]
      } yield fst + snd
  }
}