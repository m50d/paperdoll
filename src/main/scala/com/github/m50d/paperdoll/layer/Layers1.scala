package com.github.m50d.paperdoll.layer

import shapeless.{CNil, :+:, Coproduct}
import scala.annotation.implicitNotFound

/**
 * Helper for inferring type-level functions of layers
 */
@implicitNotFound("${R} is not a stack of layers")
sealed trait Layers1[R <: Coproduct, F[_[_] <: Coproduct]] {
  type O[X] <: Coproduct
  final type FO = F[O]
}
object Layers1 {
  implicit def cnil[F[_[_] <: Coproduct]] = new Layers1[CNil, F] {
    type O[X] = CNil
  }
  implicit def ccons[H <: Layer, T <: Coproduct, F[_[_] <: Coproduct]](implicit t: Layers1[T, F]) =
    new Layers1[H :+: T, F] {
      type O[X] = H#F[X] :+: t.O[X]
    }
}