package com.github.m50d.paperdoll.queue

import scalaz.Forall
import com.github.m50d.paperdoll.{Pair, Pair_, FunctionKK}

/**
 * Type aligned queue of exactly one or two elements
 */
sealed trait MiniQueue[C[_, _], A, B] {
  def fold[Z](one: C[A, B] => Z, pair: Pair[C, A, B] => Z): Z
  def asQueue: Queue[C, A, B]
  def map[D[_, _]](f: FunctionKK[C, D]): MiniQueue[D, A, B]
}

object MiniQueue {
  def one[C[_, _], A, B](a: C[A, B]): MiniQueue[C, A, B] = new MiniQueue[C, A, B] {
    override def fold[Z](one: C[A, B] => Z, pair: Pair[C, A, B] => Z) =
      one(a)
    override def asQueue = One(a)
    override def map[D[_, _]](f: FunctionKK[C, D]) = one(f(a))
  }
  def pair[C[_, _], A, B](a: Pair[C, A, B]): MiniQueue[C, A, B] = new MiniQueue[C, A, B] {
    override def fold[Z](one: C[A, B] => Z, pair: Pair[C, A, B] => Z) =
      pair(a)
    override def asQueue = a.fold(new Forall[({ type L[Z] = (C[A, Z], C[Z, B]) => Queue[C, A, B] })#L] {
      override def apply[Z] = {
        (a, b) =>
          Node(one(a), Empty[Pair_[C]#O, Z](), one(b))
      }
    })
    override def map[D[_, _]](f: FunctionKK[C, D]) = pair(a map f)
  }
}