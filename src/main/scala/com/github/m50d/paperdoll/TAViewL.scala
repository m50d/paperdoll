package com.github.m50d.paperdoll

import scalaz.Leibniz.===

/**
 * A view of the head of a type-aligned datastructure
 * S[C, X, Y]
 * Either empty (in which case X === Y)
 * or a head element C[X, A] and a tail S[C, A, Y]
 * for some unknown type A
 */
sealed trait TAViewL[S[_[_, _], _, _], C[_, _], X, Y]

/**
 * Nil - witness can be used as type-level evidence that X and Y are the same type
 */
final case class TAEmptyL[S[_[_, _], _, _], C[_, _], X, Y]()(implicit val witness: Y === X) extends TAViewL[S, C, X, Y]
/**
 * Cons - e is the head, S is the tail
 */
final case class :<[S[_[_, _], _, _], C[_, _], X, A0, Y](e: C[X, A0], s: S[C, A0, Y]) extends TAViewL[S, C, X, Y] {
  type A = A0
}