package com.github.m50d.paperdoll.layer

import shapeless.{ Coproduct, :+:, Inl, Inr, CNil }

sealed trait Member[R <: Coproduct, L <: Layers[R], R1 <: Layer] {
  type RestR <: Coproduct
  type RestL <: Layers[RestR]
  def remove[X](value: L#O[X]): Either[R1#F[X], RestL#O[X]]
}

object Member {
  implicit def nil[R1 <: Layer, R <: Coproduct](implicit rest: Layers[R]) = new Member[R1 :+: R, Layers[R1 :+: R] {
    type O[X] = R1#F[X] :+: rest.O[X]
  }, R1] {
    override type RestR = R
    override type RestL = Layers.Aux[R, rest.O] // i.e. rest.type
    def remove[X](value: R1#F[X] :+: rest.O[X]) = value match {
      case Inl(x) => Left(x)
      case Inr(r) => Right(r)
    }
  }
}