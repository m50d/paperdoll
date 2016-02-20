package com.github.m50d.paperdoll.queue

import scalaz.Forall

/**
 * A type-aligned pair of C[A, W] and C[W, B]
 * for some unknown type W.
 * Type W is deliberately inaccessible from outside
 */
sealed trait Pair[C[_, _], A, B] {
  def fold[Z](f: Forall[({ type L[W] = (C[A, W], C[W, B]) => Z })#L]): Z
}
sealed trait Pair_[C[_, _]] {
  final type O[X, Y] = Pair[C, X, Y]
}
object Pair {
  def apply[C[_, _], A, B, W](a: C[A, W], b: C[W, B]): Pair[C, A, B] = new Pair[C, A, B] {
    override def fold[Z](f: Forall[({ type L[W] = (C[A, W], C[W, B]) => Z })#L]) =
      f[W](a, b)
  }
}