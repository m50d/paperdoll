package com.github.m50d.paperdoll.queue

import scalaz.{Forall, Leibniz}
import scalaz.Leibniz.===

/**
 * A view of the head of a type-aligned datastructure
 * S[C, X, Y]
 * Either nil (in which case X === Y)
 * or a head element C[X, A] and a tail S[C, A, Y]
 * for some unknown type A
 */
sealed trait TAViewL[S[_[_, _], _, _], C[_, _], X, Y] {
  def fold[A](nil: (Y === X) => A, cons: Forall[({type L[W] = (C[X, W], S[C, W, Y]) => A})#L]): A
}

object TAViewL {
  def nil[S[_[_, _], _, _], C[_, _], X]: TAViewL[S, C, X, X] = new TAViewL[S, C, X, X] {
    override def fold[A](nil: (X === X) => A, cons: Forall[({type L[W] = (C[X, W], S[C, W, X]) => A})#L]) =
      nil(Leibniz.refl[X])
  }
  def cons[S[_[_, _], _, _], C[_, _], X, W, Y](head: C[X, W], tail: S[C, W, Y]) = new TAViewL[S, C, X, Y] {
    override def fold[A](nil: (Y === X) => A, cons: Forall[({type L[W] = (C[X, W], S[C, W, Y]) => A})#L]) =
      cons.apply[W](head, tail)
  }
}