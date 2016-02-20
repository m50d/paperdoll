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
  final def ++[X](r: Queue[C, B, X]): Queue[C, A, X] = destructureHead.fold(
    { witness => witness.subst[({ type L[V] = Queue[C, V, X] })#L](r) },
    new Forall[({ type L[W] = (C[A, W], Queue[C, W, B]) => Queue[C, A, X] })#L] {
      override def apply[W] = {
        (head, tail) =>
          head :: (tail ++ r)
      }
    })
}
object Queue {
  def empty[C[_, _], A]: Queue[C, A, A] = Empty()
  def one[C[_, _], A, B](a: C[A, B]): Queue[C, A, B] = One(a)
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
private[queue] final case class One[C[_, _], A, B](a: C[A, B]) extends Queue[C, A, B] {
  override def :+[Z](e: C[B, Z]) =
    QN[C, A, Z, B, B](MiniQueue.one(a), Empty[Pair_[C]#O, B](), MiniQueue.one(e))
  override def destructureHead = DestructuredHead.cons(a, Empty())
}
private[queue] final case class QN[C[_, _], A, B0, X, Y](
  l: MiniQueue[C, A, X], m: Queue[Pair_[C]#O, X, Y], r: MiniQueue[C, Y, B0]) extends Queue[C, A, B0] {
  override def :+[Z](e: C[B0, Z]) =
    r.fold({
      a => QN(l, m, MiniQueue.pair(Pair(a, e)))
    },
      {
        a => QN(l, m :+ a, MiniQueue.one(e))
      })

  override def destructureHead = l.fold({
    a =>
      def shiftLeft[A, B3, W](q: Queue[Pair_[C]#O, A, W], r: MiniQueue[C, W, B3]): Queue[C, A, B3] =
        q.destructureHead.fold({
          witness => witness.subst[({ type L[V] = MiniQueue[C, V, B3] })#L](r).asQueue
        }, new Forall[({ type L[V] = (Pair[C, A, V], Queue[Pair_[C]#O, V, W]) => Queue[C, A, B3] })#L] {
          override def apply[V] = {
            (head, tail) =>
              QN(MiniQueue.pair(head), tail, r)
          }
        })
      DestructuredHead.cons(a, shiftLeft(m, r))
  },
    {
      _.fold(new Forall[({ type L[W] = (C[A, W], C[W, X]) => DestructuredHead[Queue, C, A, B0] })#L] {
        override def apply[W] = {
          (a, b) =>
            DestructuredHead.cons(a, QN(MiniQueue.one[C, W, X](b), m, r))
        }
      })
    })
}