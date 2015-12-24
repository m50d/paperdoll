package com.github.m50d.paperdoll

import shapeless.Coproduct
import scalaz.Monad
import shapeless.CNil
import shapeless.:+:
import aliases._
import shapeless.Coproduct
import shapeless.ops.coproduct.Inject
import scalaz.Leibniz
import scalaz.syntax.monad._
import scalaz.Forall
import scalaz.Unapply

object aliases {
  type Arr[R <: Coproduct, A, B] = A => Eff[R, B]
  sealed trait Arr_[R <: Coproduct] {
    final type O[A, B] = Arr[R, A, B]
  }
  type Arrs[R <: Coproduct, A, B] = Queue[Arr_[R]#O, A, B]
}

sealed trait Eff[R <: Coproduct, A]
final case class Pure[R <: Coproduct, A](a: A) extends Eff[R, A]
/*sealed*/ trait Impure[R <: Coproduct, A] extends Eff[R, A] {
  //The composed layer type
  type L <: Layers[R]
  //The type of the value of the lazy intermediate step
  type X
  //The immediate value, an X inside an effect
  val eff: L#O[X]
  //The continuation from X to the next step
  val step: Arrs[R, X, A]
}

sealed trait Eff_[R <: Coproduct] {
  final type O[A] = Eff[R, A]
}

sealed trait Layer {
  type F[X]
}

sealed trait Layers[R <: Coproduct] {
  type O[X] <: Coproduct
}
object Layers {
  implicit object cnil extends Layers[CNil] {
    type O[X] = CNil
  }
  implicit def ccons[H <: Layer, T <: Coproduct](implicit t: Layers[T]) =
    new Layers[H :+: T] {
      type O[X] = H#F[X] :+: t.O[X]
    }
  type Aux[R <: Coproduct, F[_] <: Coproduct] = Layers[R] {
    type O[X] = F[X]
  }
}

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

  implicit def unapplyEff[TC[_[_]], R <: Coproduct, A0](
    implicit instance: TC[Eff_[R]#O]) = new Unapply[TC, Eff[R, A0]] {
    override type A = A0
    override type M[X] = Eff[R, X]
    override val TC = instance
    override val leibniz = Leibniz.refl[Eff[R, A0]]
  }

  def send[F[_], R <: Coproduct, N[_] <: Coproduct, V](value: F[V])(implicit l: Layers[R] { type O[X] = N[X] }, inj: Inject[N[V], F[V]]): Eff[R, V] =
    new Impure[R, V] {
      type L = Layers[R] { type O[Y] = N[Y] }
      type X = V
      val eff = Coproduct[N[V]](value)
      val step = Q0[Arr_[R]#O, V]()
    }

  def qApp[R <: Coproduct, B, W](arrs: Arrs[R, B, W]): Arr[R, B, W] =
    arrs.tviewl match {
      case e: TAEmptyL[Queue, Arr_[R]#O, B, W] => {
        b => Leibniz.symm[Nothing, Any, W, B](e.witness).apply(b).point[Eff_[R]#O]
      }
      case kt: :<[Queue, Arr_[R]#O, B, _, W] =>
        def bind[A, B](eff: Eff[R, A], k: Arrs[R, A, B]): Eff[R, B] =
          eff match {
            case Pure(y) => qApp(k)(y)
            case imp: Impure[R, A] =>
              new Impure[R, B] {
                type L = imp.L
                type X = imp.X
                val eff = imp.eff
                val step = imp.step >< k
              }
          }
        { x => bind(kt.e(x), kt.s) }
    }

  def qcomp[R1 <: Coproduct, R2 <: Coproduct, A, B, C](arrs: Arrs[R1, A, B], func: Eff[R1, B] => Eff[R2, C]): Arr[R2, A, C] =
    qApp(arrs) andThen func

  def handleRelay[R1, R <: Coproduct, A, W, M[_] <: Coproduct, T[_]](
    ret: Arr[R, A, W],
    bind: Forall[({
      type L[V] = (T[V], Arr[R, V, W]) => Eff[R, W]
    })#L]): Eff[R1 :+: R, A] => Eff[R, W] = {
    case Pure(x) => ret(x)
    case imp: Impure[R1 :+: R, A] {
      type L = Layers[R1 :+: R] {
        type O[X] = T[X] :+: M[X]
      }
    } =>
      val k = qcomp(imp.step, handleRelay[R1, R, A, W, M, T](ret, bind))
      imp.eff.removeElem[T[imp.X]] match {
        case Left(x) => bind.apply(x, k)
        case Right(u) =>
          new Impure[R, W] {
            override type L = Layers.Aux[R, M]
            override type X = imp.X
            override val eff = u
            override val step = Q1[Arr_[R]#O, X, W](k)
          }
      }
  }
}