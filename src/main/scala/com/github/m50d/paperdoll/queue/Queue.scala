package com.github.m50d.paperdoll.queue

import scalaz.Forall

/**
 * A type-aligned queue C[A, X] :: C[X, Y] :: ... :: C[Z, B]
 * Implemented as a kind of lazy binary tree: a Queue is either empty,
 * one element, or two ends and a queue of queues in the middle.
 * Theoretically this provides amortised O(1) append and take operations:
 * we only have to increase/decrease the depth to N once every 2^N
 * operations.
 * TODO: Benchmark and/or automated test to confirm this implementation is actually O(1)
 * and not accidentally much slower due to implementation errors.
 * The code was ported from Haskell (a lazy language) at a time when I didn't
 * really understand it, and so may have issues with eager evaluation in Scala
 * Also untested usage patterns might result in stack overflows for the same reason.
 */
sealed trait Queue[C[_, _], A, B] {
  /**
   * Append a single element e onto the end of this queue
   */
  def |>[Z](e: C[B, Z]): Queue[C, A, Z]
  /**
   * Either empty, or head and tail. This is a basic operation that would be really useful
   * to have on ordinary List, but I don't know the name for it
   */
  def tviewl: TAViewL[Queue, C, A, B]
  /**
   * Prepend a single element l onto the beginning of this queue
   */
  def <|:[X](l: C[X, A]): Queue[C, X, B] = Q1(l) >< this
  /**
   * Append a queue R onto the end of this queue
   */
  def ><[X](r: Queue[C, B, X]): Queue[C, A, X] = tviewl.fold(
    { witness => witness.subst[({ type L[V] = Queue[C, V, X] })#L](r) },
    new Forall[({ type L[W] = (C[A, W], Queue[C, W, B]) => Queue[C, A, X] })#L] {
      override def apply[W] = {
        (head, tail) =>
          head <|: (tail >< r)
      }
    })
}
/**
 * An empty queue - note that this implies A === B at the type level
 */
final case class Q0[C[_, _], A]() extends Queue[C, A, A] {
  override def |>[Z](e: C[A, Z]) = Q1(e)
  override def tviewl = TAViewL.nil
  override def <|:[X](l: C[X, A]): Queue[C, X, A] =
    Q1(l)
}
/**
 * A 1-element queue
 */
final case class Q1[C[_, _], A, B](a: C[A, B]) extends Queue[C, A, B] {
  override def |>[Z](e: C[B, Z]) =
    QN[C, A, Z, B, B](MiniQueue.one(a), Q0[Pair_[C]#O, B](), MiniQueue.one(e))
  override def tviewl = TAViewL.cons(a, Q0())
}
final case class QN[C[_, _], A, B0, X, Y](
  l: MiniQueue[C, A, X], m: Queue[Pair_[C]#O, X, Y], r: MiniQueue[C, Y, B0]) extends Queue[C, A, B0] {
  override def |>[Z](e: C[B0, Z]) =
    r.fold({
      a => QN(l, m, MiniQueue.pair(Pair(a, e)))
    },
      {
        a => QN(l, m |> a, MiniQueue.one(e))
      })

  override def tviewl = l.fold({
    a =>
      def buf2queue[Z, W](b: MiniQueue[C, Z, W]): Queue[C, Z, W] =
        b.fold(
          Q1.apply,
          _.fold(new Forall[({ type L[V] = (C[Z, V], C[V, W]) => Queue[C, Z, W] })#L] {
            override def apply[W] = {
              (a, b) =>
                QN(MiniQueue.one(a), Q0[Pair_[C]#O, W](), MiniQueue.one(b))
            }
          }))
      def shiftLeft[A, B3, W](q: Queue[Pair_[C]#O, A, W], r: MiniQueue[C, W, B3]): Queue[C, A, B3] =
        q.tviewl.fold({
          witness => buf2queue(witness.subst[({ type L[V] = MiniQueue[C, V, B3] })#L](r))
        }, new Forall[({ type L[V] = (Pair[C, A, V], Queue[Pair_[C]#O, V, W]) => Queue[C, A, B3] })#L] {
          override def apply[V] = {
            (head, tail) =>
              QN(MiniQueue.pair(head), tail, r)
          }
        })
      TAViewL.cons(a, shiftLeft(m, r))
  },
    {
      _.fold(new Forall[({ type L[W] = (C[A, W], C[W, X]) => TAViewL[Queue, C, A, B0] })#L] {
        override def apply[W] = {
          (a, b) =>
            TAViewL.cons(a, QN(MiniQueue.one[C, W, X](b), m, r))
        }
      })
    })
}