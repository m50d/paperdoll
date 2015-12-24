package com.github.m50d.paperdoll

import org.junit.Test
import scalaz.syntax.monad._
import shapeless.{CNil, :+:, Coproduct}
import example.Reader_
import scalaz.Unapply
import scalaz.Leibniz

class ReaderTest {
  implicit def unapplyEff[TC[_[_]], R <: Coproduct, A0](
		  implicit instance: TC[Eff_[R]#O]
  ) = new Unapply[TC, Eff[R, A0]]{
    override type A = A0
    override type M[X] = Eff[R, X]
    override val TC = instance
    override val leibniz = Leibniz.refl[Eff[R, A0]]
  }

  @Test def basicFunctionality(): Unit = {
    val reader =
      for {
        fst <- example.askReaderOnly[Int]
        snd <- example.askReaderOnly[Int]
      } yield fst + snd
  }
}