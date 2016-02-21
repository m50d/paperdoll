package com.github.m50d.paperdoll.layer

import shapeless.Coproduct

sealed trait Member[R <: Coproduct, L <: Layers[R], R1 <: Layer] {
  type RestR <: Coproduct
  type RestL <: Layers[RestR]
  def remove[X](value: L#O[X]): Either[R1#F[X], RestL#O[X]]
}

object Member {
  
}