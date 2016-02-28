package com.github.m50d.paperdoll.layer

import shapeless.{ Coproduct, :+:, Inl, Inr, CNil }

sealed trait Member[R <: Coproduct, L <: Layers[R], R1 <: Layer] {
  type RestR <: Coproduct
  type RestL <: Layers[RestR]
  def remove[X](value: L#O[X]): Either[R1#F[X], RestL#O[X]]
}

object Member {
  type CNil_[X] = CNil
  implicit def baseBase[R1 <: Layer] = new Member[R1 :+: CNil, Layers[R1 :+: CNil] {
    type O[X] = R1#F[X] :+: CNil
  }, R1] {
    override type RestR = CNil
    override type RestL = Layers.Aux[CNil, CNil_]
    def remove[X](value: R1#F[X] :+: CNil) = value match {
      case Inl(x) => Left(x)
      case Inr(cn) => Right(cn)
    }
  }
}