package com.github.m50d.paperdoll

import shapeless.Coproduct
import shapeless.CNil
import shapeless.:+:
import shapeless.ops.coproduct.Inject
import aliases._
import scalaz.syntax.monad._
import scala.annotation.tailrec
import shapeless.ops.coproduct.Remove
import scalaz.Forall
import scalaz.Leibniz

sealed trait Reader[I, X]
case class Get[I, X](val ev: Leibniz.===[I, X]) extends Reader[I, X]

object example {
  final type Reader_[I] = Layer {
    type F[X] = Reader[I, X]
  }

  def ask[I, R <: Coproduct, F[_] <: Coproduct](implicit l: Layers.Aux[R, F], inj: Inject[F[I], Reader_[I]#F[I]]): Eff[R, I] =
    Eff.send[Reader_[I]#F, R, F, I](Get[I, I](Leibniz.refl))

  def askReaderOnly[I]: Eff[Reader_[I] :+: CNil, I] =
    ask[I, Reader_[I] :+: CNil, ({ type L[X] = Reader[I, X] :+: CNil })#L]

  def addGet[R <: Coproduct, F[_] <: Coproduct](x: Int)(
    implicit l: Layers.Aux[R, F], inj: Inject[F[Int], Reader_[Int]#F[Int]]): Eff[R, Int] =
    Eff.monadEff.bind(ask[Int, R, F](l, inj)) { i => (i + x).point[Eff_[R]#O] }

  def runReader[I, R <: Coproduct, A, M[_] <: Coproduct](i: I, e: Eff[Reader_[I] :+: R, A])(implicit l: Layers.Aux[R, M]): Eff[R, A] =
    Eff.handleRelay[Reader_[I], R, A, M, Reader_[I]#F]({ _.point[Eff_[R]#O] },
      new Forall[({
        type L[V] = (Reader[I, V], Arr[R, V, A]) => Eff[R, A]
      })#L] {
        override def apply[V] = {
          (reader: Reader[I, V], arr: Arr[R, V, A]) =>
            reader match {
              case Get(ev) =>
                arr(ev(i))
            }
        }
      }).apply(e)
}