package com.github.m50d.paperdoll

import shapeless.Coproduct
import shapeless.CNil
import shapeless.:+:
import shapeless.ops.coproduct.Inject
import scalaz.syntax.monad._
import scala.annotation.tailrec
import shapeless.ops.coproduct.Remove
import scalaz.Forall
import scalaz.Leibniz

sealed trait Reader[I, X]
case class Get[I, X](val ev: Leibniz.===[I, X]) extends Reader[I, X]

object Reader {
  def ask[I, R <: Coproduct, F[_] <: Coproduct](implicit l: Layers.Aux[R, F], inj: Inject[F[I], Reader_[I]#F[I]]): Eff[R, Layers.Aux[R, F], I] =
    Eff.send[Reader_[I]#F, R, F, I](Get[I, I](Leibniz.refl))
  def askReaderOnly[I]: Eff[Reader_[I] :+: CNil, Layers[Reader_[I] :+: CNil] { type O[X] = Reader[I, X] :+: CNil }, I] =
    ask[I, Reader_[I] :+: CNil, ({ type L[X] = Reader[I, X] :+: CNil })#L]
  def addGet[R <: Coproduct, F[_] <: Coproduct](x: Int)(
    implicit l: Layers.Aux[R, F], inj: Inject[F[Int], Reader_[Int]#F[Int]]): Eff[R, Layers.Aux[R, F], Int] =
    Eff.monadEff.bind(ask[Int, R, F](l, inj)) { i => (i + x).point[Eff_[R, Layers.Aux[R, F]]#O] }
  def runReader[I, R <: Coproduct, A, M[_] <: Coproduct](i: I, e: Eff[Reader_[I] :+: R, Layers[Reader_[I] :+: R] {
    type O[X] = Reader[I, X] :+: M[X]
  }, A])(implicit l: Layers.Aux[R, M]): Eff[R, Layers.Aux[R, M], A] =
    Eff.handleRelay[Reader_[I], R, A, M, Reader_[I]#F]({ _.point[Eff_[R, Layers.Aux[R, M]]#O] },
      new Forall[({
        type L[V] = (Reader[I, V], Arr[R, Layers.Aux[R, M], V, A]) => Eff[R, Layers.Aux[R, M], A]
      })#L] {
        override def apply[V] = {
          (reader: Reader[I, V], arr: Arr[R, Layers.Aux[R, M], V, A]) =>
            reader match {
              case Get(ev) =>
                arr(ev(i))
            }
        }
      }).apply(e)
}