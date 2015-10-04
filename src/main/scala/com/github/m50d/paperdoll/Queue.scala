package com.github.m50d.paperdoll

/**
 * Function1 for types of kind [_, _]
 */
trait FunctionKK[C[_, _], D[_, _]] {
  def apply[X, Y](c: C[X, Y]): D[X, Y]
}

sealed trait P[C[_, _], A, B]
final case class CS[C[_, _], A, B, W](v1: C[A, W], v2: C[W, B]) extends P[C, A, B]

sealed trait B[C[_, _], A, B]
final case class B1[C[_, _], A, B0](v: C[A, B0]) extends B[C, A, B0]
final case class B2[C[_, _], A, B0](v: P[C, A, B0]) extends B[C, A, B0]

sealed trait Queue[C[_, _], A, B] {
  def |>[Z](e: C[B, Z]): Queue[C, A, Z]
}
final case class Q0[C[_, _], A]() extends Queue[C, A, A] {
  override def |>[Z](e: C[A, Z]) = Q1(e) 
}
final case class Q1[C[_, _], A, B](v: C[A, B]) extends Queue[C, A, B] {
  override def  |>[Z](e: C[B, Z]) =
    QN[C, A, Z, B, B](B1(v), Q0[({ type L[A, B] = P[C, A, B] })#L, B](), B1(e))
}
final case class QN[C[_, _], A, B0, X, Y](
  v1: B[C, A, X], v2: Queue[({ type L[A, B] = P[C, A, B] })#L, X, Y], v3:  B[C, Y, B0]) extends Queue[C, A, B0] {
  override def |>[Z](e: C[B0, Z]) =
    v3 match {
    case B1(a) => QN(v1, v2, B2(CS(a, e)))
    case B2(r) => QN(v1, v2 |> r, B1(e))
  }
}

object Queue {
  def tmapp[C[_, _], D[_, _]](f: FunctionKK[C, D]): FunctionKK[({ type L[X, Y] = (P[C, X, Y]) })#L, ({ type L[X, Y] = (P[D, X, Y]) })#L] =
    new FunctionKK[({ type L[X, Y] = (P[C, X, Y]) })#L, ({ type L[X, Y] = (P[D, X, Y]) })#L] {
      def apply[X, Y](phi: P[C, X, Y]): P[D, X, Y] =
        phi match {
          case CS(v1, v2) ⇒ CS(f.apply(v1), f.apply(v2))
        }
    }
  def tmapb[C[_, _], D[_, _]](f: FunctionKK[C, D]): FunctionKK[({ type L[X, Y] = (B[C, X, Y]) })#L, ({ type L[X, Y] = (B[D, X, Y]) })#L] =
    new FunctionKK[({ type L[X, Y] = (B[C, X, Y]) })#L, ({ type L[X, Y] = (B[D, X, Y]) })#L] {
      def apply[X, Y](phi: B[C, X, Y]) =
        phi match {
          case B1(v) ⇒ B1(f.apply(v))
          case B2(v) ⇒ B2(tmapp(f)(v))
        }
    }
}