package paperdoll.queue

/**
 * A type-aligned pair of C[A, X] and C[X, B]
 * for some unknown type X.
 */
sealed trait Pair[C[_, _], A, B]

object Pair {
  final case class Impl[C[_, _], A, B, X](a: C[A, X], b: C[X, B]) extends Pair[C, A, B]  
  def apply[C[_, _], A, B, X](a: C[A, X], b: C[X, B]): Pair[C, A, B] = Impl(a, b)
}