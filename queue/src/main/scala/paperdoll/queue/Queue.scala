package paperdoll.queue

import scalaz.Forall
import scalaz.Leibniz

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
   * (i.e. no recursion). Most callers end up recursing in one way or another,
   * but I've found it too difficult to abstract over the type differences enough to factor
   * out this common recursion.
   * This is a basic operation that would be really useful
   * to have on ordinary List, but I'm not aware of it having a standard name
   */
  def destructureHead: DestructuredHead[Queue, C, A, B]
}
object Queue {
  /**
   * Empty queue - note that this implies A === B at the type level
   */
  final case class Empty[C[_, _], A]() extends Queue[C, A, A] {
    override def :+[Z](e: C[A, Z]) = One(e)
    override def destructureHead = DestructuredHead.Nil(Leibniz.refl)
  }
  final case class One[C[_, _], A, B](value: C[A, B]) extends Queue[C, A, B] {
    override def :+[Z](e: C[B, Z]) =
      Node[C, A, Z, B, B](MiniQueue.One(value), Empty[Pair[C, ?, ?], B](), MiniQueue.One(e))
    override def destructureHead = DestructuredHead.Cons(value, Empty())
  }
  final case class Node[C[_, _], A, B, X, Y](
    head: MiniQueue[C, A, X], middle: Queue[Pair[C, ?, ?], X, Y], last: MiniQueue[C, Y, B]) extends Queue[C, A, B] {
    override def :+[Z](newLast: C[B, Z]) = last match {
      case MiniQueue.One(lastOne) => Node(head, middle, MiniQueue.Pair(Pair(lastOne, newLast)))
      case MiniQueue.Pair(lastPair) ⇒ Node(head, middle :+ lastPair, MiniQueue.One(newLast))
    }

    override def destructureHead = head match {
      case MiniQueue.One(headOne) ⇒
        DestructuredHead.Cons(headOne, middle.destructureHead match {
          case nil: DestructuredHead.Nil[Queue, Pair[C, ?, ?], X, Y] => nil.witness.subst[MiniQueue[C, ?, B]](last).asQueue
          case cons: DestructuredHead.Cons[Queue, Pair[C, ?, ?], X, Y, w] => Node(MiniQueue.Pair(cons.head), cons.tail, last)
        })
      case MiniQueue.Pair(Pair.Impl(first, second)) =>
        DestructuredHead.Cons(first, Node(MiniQueue.One(second), middle, last))
    }
  }
}