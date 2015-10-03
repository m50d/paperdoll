package com.github.m50d.paperdoll

sealed trait P[C[_, _], A, B]
case class CS[C[_, _], A, B, W](v: C[A, W] => C[W, B]) extends P[C, A, B]

sealed trait B[C[_, _], A, B]
case class B1[C[_, _], A, B0](v: C[A, B0]) extends B[C, A, B0]
case class B2[C[_, _], A, B0](v: P[C, A, B0]) extends B[C, A, B0]

sealed trait Queue[C[_, _], A, B]
case class Q0[C[_, _], A]() extends Queue[C, A, A]
case class Q1[C[_, _], A, B](v: C[A, B]) extends Queue[C, A, B]
case class QN[C[_, _], A, B0, X, Y](
    v: (B[C, A, X], Queue[({type L[A, B] = P[C, A, B]})#L, X, Y] => B[C, Y, B])