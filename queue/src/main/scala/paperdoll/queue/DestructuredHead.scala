package paperdoll.queue

import scalaz.Leibniz.===

/**
 * Destructure of a type-aligned datastructure S[C, X, Y] from the left
 * Either nil (in which case X === Y)
 * or a head element C[X, A] and a tail S[C, A, Y]
 * for some unknown type A (which is deliberately not exposed outside)
 * Arguably excessively generic (since we only actually use this with S = Queue)
 */
sealed trait DestructuredHead[S[_[_, _], _, _], C[_, _], X, Y]

object DestructuredHead {
  final case class Nil[S[_[_, _], _, _], C[_, _], X, Y](witness: Y === X) extends DestructuredHead[S, C, X, Y]
  final case class Cons[S[_[_, _], _, _], C[_, _], X, Y, W](head: C[X, W], tail: S[C, W, Y]) extends DestructuredHead[S, C, X, Y]
}