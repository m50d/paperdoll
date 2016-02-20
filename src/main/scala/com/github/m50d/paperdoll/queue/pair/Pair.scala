package com.github.m50d.paperdoll.queue.pair

import scalaz.Forall

/**
 * A type-aligned pair of C[A, W] and C[W, B]
 * for some unknown type W.
 * Interface and impl deliberately separated
 * so that the type W is not observable
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
      f.apply[W].apply(a, b)
  }
}