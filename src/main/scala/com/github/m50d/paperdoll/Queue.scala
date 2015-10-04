package com.github.m50d.paperdoll

import scalaz.Leibniz.===

/**
 * Function1 for types of kind [_, _]
 */
trait FunctionKK[C[_, _], D[_, _]] {
  def apply[X, Y](c: C[X, Y]): D[X, Y]
}

sealed trait TAViewL[S[_[_, _], _, _], C[_, _], X, Y]

final case class TAEmptyL[S[_[_, _], _, _], C[_, _], X, Y]()(implicit val witness: Y === X) extends TAViewL[S, C, X, Y]
final case class :<[S[_[_, _], _, _], C[_, _], X, Y0, Z](e: C[X, Y0], s: S[C, Y0, Z]) extends TAViewL[S, C, X, Z] {
  type Y = Y0
}

sealed trait P[C[_, _], A, B]

/**
 * Partially applied P to make it easier to write types
 */
sealed trait P_[C[_, _]] {
 final type O[X, Y] = P[C, X, Y] 
}

final case class CS[C[_, _], A, B, W0](a: C[A, W0], b: C[W0, B]) extends P[C, A, B] {
  type W = W0
}

sealed trait B[C[_, _], A, B]
final case class B1[C[_, _], A, B0](a: C[A, B0]) extends B[C, A, B0]
final case class B2[C[_, _], A, B0](v: P[C, A, B0]) extends B[C, A, B0]

sealed trait Queue[C[_, _], A, B] {
  def |>[Z](e: C[B, Z]): Queue[C, A, Z]
  def tviewl: TAViewL[Queue, C, A, B]
}
final case class Q0[C[_, _], A]() extends Queue[C, A, A] {
  override def |>[Z](e: C[A, Z]) = Q1(e) 
  override def tviewl = TAEmptyL()
}
final case class Q1[C[_, _], A, B](a: C[A, B]) extends Queue[C, A, B] {
  override def  |>[Z](e: C[B, Z]) =
    QN[C, A, Z, B, B](B1(a), Q0[P_[C]#O, B](), B1(e))
  override def tviewl = :<(a, Q0())
}
final case class QN[C[_, _], A, B0, X, Y](
  l: B[C, A, X], m: Queue[P_[C]#O, X, Y], r:  B[C, Y, B0]) extends Queue[C, A, B0] {
  override def |>[Z](e: C[B0, Z]) =
    r match {
    case B1(a) => QN(l, m, B2(CS(a, e)))
    case B2(r) => QN(l, m |> r, B1(e))
  }
  override def tviewl = l match {
    case B2(CS(a, b)) => :<(a, QN(B1(b), m, r))
    case B1(a) => {
      def buf2queue[Z, W](b: B[C, Z, W]): Queue[C, Z, W] = b match {
        case B1(a) => Q1(a)
        case B2(cs @ CS(a, b)) => QN(B1(a), Q0[P_[C]#O, cs.W](), B1(b))
      } 
//      def shiftLeft[A, B3, W](q: Queue[({ type L[A, B] = P[C, A, B] })#L, A, W], r: B[C, W, B3]): Queue[C, A, B3] =
//        q.tviewl match {
//        case tael @ TAEmptyL() => buf2queue(tael.witness.subst[({type L[V] = B[C, V, B3]})#L](r))
//        case cl @ :<(l, m) => QN(B2(l), m, r)
//      }
      ???
    }
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