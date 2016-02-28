package com.github.m50d.paperdoll

import scalaz.{Forall, Leibniz}
import scalaz.Leibniz.===

/**
 * Destructure of a type-aligned datastructure S[C, X, Y] from the left
 * Either nil (in which case X === Y)
 * or a head element C[X, A] and a tail S[C, A, Y]
 * for some unknown type A (which is deliberately not exposed outside)
 * Arguably excessively generic (since we only actually use this with S = Queue)
 */
sealed trait DestructuredHead[S[_[_, _], _, _], C[_, _], X, Y] {
  def fold[A](nil: (Y === X) => A, cons: Forall[({type L[W] = (C[X, W], S[C, W, Y]) => A})#L]): A
}

object DestructuredHead {
  def nil[S[_[_, _], _, _], C[_, _], X]: DestructuredHead[S, C, X, X] = new DestructuredHead[S, C, X, X] {
    override def fold[A](nil: (X === X) => A, cons: Forall[({type L[W] = (C[X, W], S[C, W, X]) => A})#L]) =
      nil(Leibniz.refl[X])
  }
  def cons[S[_[_, _], _, _], C[_, _], X, W, Y](head: C[X, W], tail: S[C, W, Y]) = new DestructuredHead[S, C, X, Y] {
    override def fold[A](nil: (Y === X) => A, cons: Forall[({type L[W] = (C[X, W], S[C, W, Y]) => A})#L]) =
      cons.apply[W](head, tail)
  }
}