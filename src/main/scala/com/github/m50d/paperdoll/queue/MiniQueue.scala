package com.github.m50d.paperdoll.queue

/**
 * Type aligned queue of exactly one or two elements
 */
sealed trait MiniQueue[C[_, _], A, B] {
  def fold[Z](one: C[A, B] => Z, pair: Pair[C, A, B] => Z): Z
}

object MiniQueue {
  def one[C[_, _], A, B](a: C[A, B]): MiniQueue[C, A, B] = new MiniQueue[C, A, B] {
    override def fold[Z](one: C[A, B] => Z, pair: Pair[C, A, B] => Z) =
      one(a)
  }
  def pair[C[_, _], A, B](a: Pair[C, A, B]): MiniQueue[C, A, B] = new MiniQueue[C, A, B] {
    override def fold[Z](one: C[A, B] => Z, pair: Pair[C, A, B] => Z) =
      pair(a)
  }
}