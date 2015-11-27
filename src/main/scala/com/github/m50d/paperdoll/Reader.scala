package com.github.m50d.paperdoll

import shapeless.Coproduct
import shapeless.ops.coproduct.Inject
import aliases._

trait Reader[I, X]

sealed trait Reader_[I] {
  final type O[X] = Reader[I, X]
}

case class Get[I]() extends Reader[I, I]

object example {
  def ask[I, R <: Coproduct, F[_] <: Coproduct](implicit l: Layers[R]{type O[X] = F[X]}, inj: Inject[F[I], Reader_[I]#O[I]]): Eff[R, I] =
    new Impure[R, I] {
    type L = Layers[R] {type O[Y] = F[Y]}
    type X = I
    val eff = Coproduct[F[I]](Get[I](): Reader[I, I])
    val step = Q0[Arr_[R]#O, I]()
  }
}