package com.github.m50d.paperdoll

import shapeless.Coproduct

object aliases {
  type Arr[R <: Coproduct, A, B] = A => FFree[R, B]
  sealed trait Arr_[R <: Coproduct] {
    final type O[A, B] = Arr[R, A, B]
  }
  type Arrs[R <: Coproduct, A, B] = Queue[Arr_[R]#O, A, B]
}

sealed trait FFree[R <: Coproduct, A]
final case class Pure[R <: Coproduct, A](a: A) extends FFree[R, A]
final case class Impure[R <: Coproduct, X, A](
    eff: X, //No, X is still inside stockings at this point
    step: X => FFree[R, A]
) extends FFree[R, A]

sealed trait FFree_[R <: Coproduct] {
  final type O[A] = FFree[R, A]
}