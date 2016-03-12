package com.github.m50d.paperdoll

import shapeless.Coproduct
import com.github.m50d.paperdoll.queue.Queue
import com.github.m50d.paperdoll.layer.Layers

package object effect {
  /**
   * An effectful function A => B, with effects from the stack represented
   * by R and O.
   */
  type Arr[R <: Coproduct, A, B, O[_]] = A => Eff.Aux[R, B, O]
  sealed trait Arr_[R <: Coproduct, O0[_]] {
    final type O[A, B] = Arr[R, A, B, O0]
  }
  /**
   * A type-aligned queue of effectful functions.
   * The overall queue runs A => B, but there may be other intermediate types in the queue
   */
  type Arrs[R <: Coproduct, A, B, O[_]] = Queue[(Arr_[R, O])#O, A, B]
}