package com.github.m50d.paperdoll.layer

import shapeless.{ CNil, Coproduct, :+:, Inl, Inr, HNil, HList, :: }
import scala.annotation.implicitNotFound

/**
 * An effect, represented as a functor-like type F[X]
 * (Note that F does not necessarily need to be a Functor - the machinery
 * will provide suitable map and flatMap operations)
 */
sealed trait Layer {
  type F[X]
}

object Layer {
  type Aux[F0[_]] = Layer {
    type F[X] = F0[X]
  }
}

/**
 * A stack of several possible effects. Each component of R is subtype of Layer
 */
@implicitNotFound("${R} is not a stack of layers")
sealed trait Layers[R <: Coproduct, F[_[_] <: Coproduct]] {
  /**
   * The type of a concrete value of type X in this stack of layers
   */
  type O[X] <: Coproduct
  final type FO = F[O]
}
object Layers {
  implicit def cnil[F[_[_] <: Coproduct]] = new Layers[CNil, F] {
    type O[X] = CNil
  }
  implicit def ccons[H <: Layer, T <: Coproduct, F[_[_] <: Coproduct]](implicit t: Layers[T, F]) =
    new Layers[H :+: T, F] {
      type O[X] = H#F[X] :+: t.O[X]
    }
}