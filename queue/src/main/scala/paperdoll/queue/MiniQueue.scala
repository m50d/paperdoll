package paperdoll.queue

import scalaz.Forall

/**
 * Type aligned queue of exactly one or two elements
 */
sealed trait MiniQueue[C[_, _], A, B] {
  def asQueue: Queue[C, A, B]
}

object MiniQueue {
  final case class One[C[_, _], A, B](a: C[A, B]) extends MiniQueue[C, A, B] {
    override def asQueue = Queue.One(a)
  }
  final case class Pair[C[_, _], A, B](a: paperdoll.queue.Pair[C, A, B]) extends MiniQueue[C, A, B] {
    override def asQueue = a match {
      case pi: paperdoll.queue.Pair.Impl[C, A, B, x] => 
        Queue.Node(One(pi.a), Queue.Empty[paperdoll.queue.Pair[C, ?, ?], x](), One(pi.b))
      }
  }
}