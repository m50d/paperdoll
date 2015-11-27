package com.github.m50d.paperdoll

import shapeless.Coproduct
import scalaz.Monad

object aliases {
  type Arr[R <: Coproduct, A, B] = A => FFree[R, B]
  sealed trait Arr_[R <: Coproduct] {
    final type O[A, B] = Arr[R, A, B]
  }
  type Arrs[R <: Coproduct, A, B] = Queue[Arr_[R]#O, A, B]
}
import aliases._

sealed trait FFree[R <: Coproduct, A]
final case class Pure[R <: Coproduct, A](a: A) extends FFree[R, A]
final case class Impure[R <: Coproduct, X, A](
    eff: X, //No, X is still inside stockings at this point
    step: Arrs[R, X, A]
) extends FFree[R, A]

sealed trait FFree_[R <: Coproduct] {
  final type O[A] = FFree[R, A]
}

object FFree {
  implicit def monadFFree[R <: Coproduct] = new Monad[FFree_[R]#O] {
    override def point[A](a: => A) = Pure(a)
    override def bind[A, B](fa: FFree[R, A])(f: A => FFree[R, B]) =
      fa match {
      case Pure(x) => f(x)
      case Impure(u, q) => Impure(u, q |> f)
    }
  }
}