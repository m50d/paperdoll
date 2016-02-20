package com.github.m50d.paperdoll.queue

/**
 * Function1 for types of kind [_, _]
 */
trait FunctionKK[C[_, _], D[_, _]] {
  def apply[X, Y](c: C[X, Y]): D[X, Y]
}