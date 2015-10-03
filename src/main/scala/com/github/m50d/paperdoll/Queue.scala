package com.github.m50d.paperdoll

trait P[C[_, _], A, B] {
  def cs[W](f: C[A, W], s: C[W, B]): P[C, A, B]
}

//sealed trait B[C[_, _], A, B]
//trait B1[C[_, _], A, B0] extends B[C, A, B0] {
//  def 
//}