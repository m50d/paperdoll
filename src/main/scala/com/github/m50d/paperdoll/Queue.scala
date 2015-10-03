package com.github.m50d.paperdoll

sealed trait P[C[_, _], A, B] {
  def cs[W](f: C[A, W], s: C[W, B]): P[C, A, B]
}