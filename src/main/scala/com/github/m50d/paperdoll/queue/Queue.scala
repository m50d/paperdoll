package com.github.m50d.paperdoll.queue

import scalaz.Forall

/**
 * A type-aligned queue C[A, X] :: C[X, Y] :: ... :: C[Z, B]
 * Implemented as a kind of lazy binary tree: a Queue is either empty,
 * one element, or two MiniQueue ends and a queue of queues in the middle.
 * Theoretically this provides amortised O(1) :+ and take operations:
 * we only have to increase/decrease the depth to N once every 2^N
 * operations.
 * TODO: Benchmark and/or automated test to confirm this implementation is actually O(1)
 * and not accidentally much slower due to implementation errors.
 * The code was ported from Haskell (a lazy language) at a time when I didn't
 * really understand it, and so may have issues with eager evaluation in Scala
 * Also untested usage patterns might result in stack overflows for the same reason.
 */
sealed trait Queue[C[_, _], A, B] {
  def :+[Z](element: C[B, Z]): Queue[C, A, Z]
  /**
   * Examine the front of the queue, destructuring it as either the queue being empty,
   * or a head and tail. Usually followed by a .fold which acts as a shallow fold
   * (i.e. no recursion)
   * TODO: look at whether conventional fold would be more useful
   * This is a basic operation that would be really useful
   * to have on ordinary List, but I'm not aware of it having a standard name
   */
  def destructureHead: DestructuredHead[Queue, C, A, B]
  def ::[X](l: C[X, A]): Queue[C, X, B] = One(l) ++ this
  final def ++[X](other: Queue[C, B, X]): Queue[C, A, X] = destructureHead.fold(
    { witness => witness.subst[({ type L[V] = Queue[C, V, X] })#L](other) },
    new Forall[({ type L[W] = (C[A, W], Queue[C, W, B]) => Queue[C, A, X] })#L] {
      override def apply[W] = {
        (head, tail) =>
          head :: (tail ++ other)
      }
    })
}
object Queue {
  def empty[C[_, _], A]: Queue[C, A, A] = Empty()
  def one[C[_, _], A, B](value: C[A, B]): Queue[C, A, B] = One(value)
}
/**
 * Empty queue - note that this implies A === B at the type level
 */
private[queue] final case class Empty[C[_, _], A]() extends Queue[C, A, A] {
  override def :+[Z](e: C[A, Z]) = One(e)
  override def destructureHead = DestructuredHead.nil
  /**
   * Break infinite loop when using the parent trait implementation
   */
  override def ::[X](l: C[X, A]): Queue[C, X, A] = One(l)
}
private[queue] final case class One[C[_, _], A, B](value: C[A, B]) extends Queue[C, A, B] {
  override def :+[Z](e: C[B, Z]) =
    Node[C, A, Z, B, B](MiniQueue.one(value), Empty[Pair_[C]#O, B](), MiniQueue.one(e))
  override def destructureHead = DestructuredHead.cons(value, Empty())
}
private[queue] final case class Node[C[_, _], A, B0, X, Y](
  head: MiniQueue[C, A, X], middle: Queue[Pair_[C]#O, X, Y], last: MiniQueue[C, Y, B0]) extends Queue[C, A, B0] {
  override def :+[Z](newLast: C[B0, Z]) =
    last.fold({
      lastOne => Node(head, middle, MiniQueue.pair(Pair(lastOne, newLast)))
    },
      {
        lastPair => Node(head, middle :+ lastPair, MiniQueue.one(newLast))
      })

  override def destructureHead = head.fold({
    headOne =>
      def shiftForward[A, W, B](queue: Queue[Pair_[C]#O, A, W], last: MiniQueue[C, W, B]): Queue[C, A, B] =
        queue.destructureHead.fold({
          witness => witness.subst[({ type L[V] = MiniQueue[C, V, B] })#L](last).asQueue
        }, new Forall[({ type L[V] = (Pair[C, A, V], Queue[Pair_[C]#O, V, W]) => Queue[C, A, B] })#L] {
          override def apply[V] = {
            (head, tail) =>
              Node(MiniQueue.pair(head), tail, last)
          }
        })
      DestructuredHead.cons(headOne, shiftForward(middle, last))
  },
    {
      _.fold(new Forall[({ type L[W] = (C[A, W], C[W, X]) => DestructuredHead[Queue, C, A, B0] })#L] {
        override def apply[W] = {
          (first, second) =>
            DestructuredHead.cons(first, Node(MiniQueue.one[C, W, X](second), middle, last))
        }
      })
    })
}