package paperdoll.queue

import scalaz.Forall

/**
 * Type aligned queue of exactly one or two elements
 */
private[queue] sealed trait MiniQueue[C[_, _], A, B] {
  def fold[Z](one: C[A, B] ⇒ Z, pair: Pair[C, A, B] ⇒ Z): Z
  def asQueue: Queue[C, A, B]
}

private[queue] object MiniQueue {
  def one[C[_, _], A, B](a: C[A, B]): MiniQueue[C, A, B] = new MiniQueue[C, A, B] {
    override def fold[Z](one: C[A, B] ⇒ Z, pair: Pair[C, A, B] ⇒ Z) =
      one(a)
    override def asQueue = One(a)
    override def toString = f"M1($a%s)"
  }
  def pair[C[_, _], A, B](a: Pair[C, A, B]): MiniQueue[C, A, B] = new MiniQueue[C, A, B] {
    override def fold[Z](one: C[A, B] ⇒ Z, pair: Pair[C, A, B] ⇒ Z) = pair(a)
    override def asQueue = a.fold(new Forall[({ type L[Z] = (C[A, Z], C[Z, B]) ⇒ Queue[C, A, B] })#L] {
      override def apply[Z] = {
        (a, b) ⇒
          Node(one(a), Empty[Pair_[C]#O, Z](), one(b))
      }
    })
    override def toString = f"MP($a%s)"
  }
}