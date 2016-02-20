package com.github.m50d

import shapeless.Coproduct

package object paperdoll {
  /**
   * An effectful function A => B, with effects from the stack represented
   * by R and L.
   */
  type Arr[R <: Coproduct, L <: Layers[R], A, B] = A => Eff[R, L, B]
  sealed trait Arr_[R <: Coproduct, L <: Layers[R]] {
    final type O[A, B] = A => Eff[R, L, B]
  }
  /**
   * A type-aligned queue of effectful functions.
   * The overall queue runs A => B, but there may be other intermediate types in the queue
   */
  type Arrs[R <: Coproduct, L <: Layers[R], A, B] = Queue[(Arr_[R, L])#O, A, B]
  type Reader_[I] = Layer {
    type F[X] = Reader[I, X]
  }
}