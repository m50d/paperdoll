package com.github.m50d.paperdoll

import shapeless.Coproduct
import scalaz.Monad

object aliases {
  type Arr[R <: Coproduct, A, B] = A => Eff[R, B]
  sealed trait Arr_[R <: Coproduct] {
    final type O[A, B] = Arr[R, A, B]
  }
  type Arrs[R <: Coproduct, A, B] = Queue[Arr_[R]#O, A, B]
}
import aliases._

sealed trait Layers[R <: Coproduct] {
  type O[X]
}

sealed trait Eff[R <: Coproduct, A]
final case class Pure[R <: Coproduct, A](a: A) extends Eff[R, A]
sealed trait Impure[R <: Coproduct, A] extends Eff[R, A]{
  type L <: Layers[R]
  type X
    val eff: L#O[X]
    val step: Arrs[R, X, A]
}

sealed trait Eff_[R <: Coproduct] {
  final type O[A] = Eff[R, A]
}

final case class Layer[F[_]]()

object Eff {
  implicit def monadEff[R <: Coproduct] = new Monad[Eff_[R]#O] {
    override def point[A](a: => A) = Pure(a)
    override def bind[A, B](fa: Eff[R, A])(f: A => Eff[R, B]) =
      fa match {
      case Pure(x) => f(x)
      case i: Impure[R, A] => new Impure[R, B] {
        type L = i.L
        type X = i.X
        val eff = i.eff
        val step = i.step |> f 
      }
    }
  }
}