package com.github.m50d.paperdoll.layer

import shapeless.{ Coproduct, :+:, Inl, Inr }

sealed trait Member[R <: Coproduct, L <: Layers[R], R1 <: Layer] {
  type RestR <: Coproduct
  type RestL <: Layers[RestR]
  def remove[X](value: L#O[X]): Either[R1#F[X], RestL#O[X]]
}

object Member {
//  implicit def base[R <: Coproduct, L <: Layers[R], R1 <: Layer](
//    implicit l1l: Layers[R1 :+: R]) = new Member[R1 :+: R, Layers.Aux[R1 :+: R, l1l.O], R1] {
//    override type RestR = R
//    override type RestL = L
//    override def remove[X](value: l1l.O[X]) =
//      value match {
//        case Inl(x) => Left(x)
//        case Inr(r) => Right(r)
//      }
//  }
}