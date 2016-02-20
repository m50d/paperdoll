package com.github.m50d.paperdoll

import shapeless.{ CNil, Coproduct, :+: }

/**
 * An effect, represented as a functor-like type F[X]
 * (Note that F does not necessarily need to be a Functor - the machinery
 * will provide suitable map and flatMap operations)
 */
sealed trait Layer {
  type F[X]
}

/**
 * A stack of several possible effects. Each component of R is subtype of Layer 
 */
sealed trait Layers[R <: Coproduct] {
  /**
   * The functor-like type of a concrete value for this stack of layers
   */
  type O[X] <: Coproduct
}
object Layers {
  /**
   * This is sadly less useful than it might otherwise be because F's kind
   * generally means it has to be expressed as a type lambda, at which point
   * one might as well just define an explicit Layers instead
   */
  type Aux[R <: Coproduct, F[_] <: Coproduct] = Layers[R] {
    type O[X] = F[X]
  }
  implicit def cnil = new Layers[CNil] {
    type O[X] = CNil
  }
  implicit def ccons[H <: Layer, T <: Coproduct](implicit t: Layers[T]) =
    new Layers[H :+: T] {
      type O[X] = H#F[X] :+: t.O[X]
    }
}