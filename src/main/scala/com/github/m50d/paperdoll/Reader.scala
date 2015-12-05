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
  
  def ask[I, R <: Coproduct, F[_] <: Coproduct](implicit l: Layers[R]{type O[X] = F[X]}, inj: Inject[F[I], Reader_[I]#F[I]]): Eff[R, I] =
    Eff.send[Reader_[I]#F, R, F, I](Get[I]())
    
  def askReaderOnly[I]: Eff[Reader_[I] :+: CNil, I] =
    ask[I, Reader_[I] :+: CNil, ({type L[X] = Reader[I, X] :+: X :+: CNil})#L]
    
}