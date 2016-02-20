package com.github.m50d.paperdoll.queue

import scalaz.Forall

/**
 * Type aligned queue of exactly one or two elements
 */
sealed trait MiniQueue[C[_, _], A, B] {
  def fold[Z](one: C[A, B] => Z, pair: Pair[C, A, B] => Z): Z
  def asQueue: Queue[C, A, B]
}

object MiniQueue {
  def one[C[_, _], A, B](a: C[A, B]): MiniQueue[C, A, B] = new MiniQueue[C, A, B] {
    override def fold[Z](one: C[A, B] => Z, pair: Pair[C, A, B] => Z) =
      one(a)
    override def asQueue = One(a)
  }
  def pair[C[_, _], A, B](a: Pair[C, A, B]): MiniQueue[C, A, B] = new MiniQueue[C, A, B] {
    override def fold[Z](one: C[A, B] => Z, pair: Pair[C, A, B] => Z) =
      pair(a)
    override def asQueue = a.fold(new Forall[({ type L[Z] = (C[A, Z], C[Z, B]) => Queue[C, A, B] })#L] {
      override def apply[Z] = {
        (a, b) =>
          QN(one(a), Empty[Pair_[C]#O, Z](), one(b))
      }
    })
  }
}