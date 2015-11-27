package com.github.m50d.paperdoll

import shapeless.Coproduct
import shapeless.CNil
import shapeless.:+:
import shapeless.ops.coproduct.Inject
import aliases._

trait Reader[I, X]

case class Get[I]() extends Reader[I, I]

object example {
  final type Reader_[I] = Layer {
    type F[X] = Reader[I, X]
  }
  
  def send[F[_], R <: Coproduct, N[_] <: Coproduct, V](value: F[V])(implicit l: Layers[R]{type O[X] = N[X]}, inj: Inject[N[V], F[V]]): Eff[R, V] =
    new Impure[R, V] {
    type L = Layers[R] {type O[Y] = N[Y]}
    type X = V
    val eff = Coproduct[N[V]](value)
    val step = Q0[Arr_[R]#O, V]()
  }
  
  def ask[I, R <: Coproduct, F[_] <: Coproduct](implicit l: Layers[R]{type O[X] = F[X]}, inj: Inject[F[I], Reader_[I]#F[I]]): Eff[R, I] =
    send[Reader_[I]#F, R, F, I](Get[I]())
    
  def askReaderOnly[I]: Eff[Reader_[I] :+: CNil, I] =
    ask[I, Reader_[I] :+: CNil, ({type L[X] = Reader[I, X] :+: X :+: CNil})#L]
    
//    send[Reader_[I]#O, Reader_[I] :+: CNil, ({type L[X] = Reader[I, X] :+: X :+: CNil})#L, I](Get[I]())
}