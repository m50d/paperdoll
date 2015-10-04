package com.github.m50d.paperdoll

trait Forall1[C[_, _]] {
  def apply[X, Y]: C[X, Y]
}

sealed trait P[C[_, _], A, B]
case class CS[C[_, _], A, B, W](v1: C[A, W], v2: C[W, B]) extends P[C, A, B]

sealed trait B[C[_, _], A, B]
case class B1[C[_, _], A, B0](v: C[A, B0]) extends B[C, A, B0]
case class B2[C[_, _], A, B0](v: P[C, A, B0]) extends B[C, A, B0]

sealed trait Queue[C[_, _], A, B] {
  def |>[Z](e: C[B, Z]): Queue[C, A, Z]
}
case class Q0[C[_, _], A]() extends Queue[C, A, A]
case class Q1[C[_, _], A, B](v: C[A, B]) extends Queue[C, A, B]
case class QN[C[_, _], A, B0, X, Y](
  v: (B[C, A, X], Queue[({ type L[A, B] = P[C, A, B] })#L, X, Y]) ⇒ B[C, Y, B0]) extends Queue[C, A, B0]

object Queue {
  def tmapp[C[_, _], D[_, _]](f: Forall1[({type L[X, Y] = (C[X, Y]) => D[X, Y]})#L])(phi: P[C, X, Y]): P[D, X, Y] =
    phi match {
    case CS(v1, v2) => CS(f.apply(v1), f.apply(v2)) 
  }
  def tmapb[C[_, _], D[_, _]](f: Forall1[({type L[X, Y] = (C[X, Y]) => D[X, Y]})#L])(phi: B[C, X, Y]): B[C, X, Y] =
    phi match {
    case B1(v) => B1(f.apply(v))
    case B2(v) => B2(tmapp(f)(v))
  }
}