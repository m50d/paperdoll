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
  type Arr[R <: Coproduct, L <: Layers[R], A, B] = A => Eff[R, L, B]
  type Arrs[R <: Coproduct, L <: Layers[R], A, B] = Queue[(Arr_[R, L])#O, A, B]
}

sealed trait Arr_[R <: Coproduct, L <: Layers[R]] {
  final type O[A, B] = A => Eff[R, L, B]
}

sealed trait Eff[R <: Coproduct, L <: Layers[R], A] {
  def fold[B](p: A => B, i: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) => B })#K]): B
}
sealed trait Pure[R <: Coproduct, L <: Layers[R], A] extends Eff[R, L, A] {
  val a: A
  override def fold[B](p: A => B, i: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) => B })#K]) = p(a)
}
sealed trait Impure[R <: Coproduct, L <: Layers[R], A] extends Eff[R, L, A] {
  //The type of the value of the lazy intermediate step
  type X
  //The immediate value, an X inside an effect
  val eff: L#O[X]
  //The continuation from X to the next step
  val step: Arrs[R, L, X, A]

  override def fold[B](p: A => B, i: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) => B })#K]) =
    i.apply[X](eff, step)
}

sealed trait Eff_[R <: Coproduct, L <: Layers[R]] {
  final type O[A] = Eff[R, L, A]
}

sealed trait Layer {
  type F[X]
}

sealed trait Layers[R <: Coproduct] {
  type O[X] <: Coproduct
}
trait Layers1 {
  implicit object cnil extends Layers[CNil] {
    type O[X] = CNil
  }
}
object Layers extends Layers1 {
  implicit def ccons[H <: Layer, T <: Coproduct](implicit t: Layers[T]) =
    new Layers[H :+: T] {
      type O[X] = H#F[X] :+: t.O[X]
    }
  type Aux[R <: Coproduct, F[_] <: Coproduct] = Layers[R] {
    type O[X] = F[X]
  }
}

object Eff {
  implicit def monadEff[R <: Coproduct, L <: Layers[R]] = new Monad[(Eff_[R, L])#O] {
    override def point[A](a0: => A) = new Pure[R, L, A] { val a = a0 }
    override def bind[A, B](fa: Eff[R, L, A])(f: A => Eff[R, L, B]) =
      fa.fold[Eff[R, L, B]](f, new Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) => Eff[R, L, B] })#K] {
        override def apply[X0] = {
          (eff0, step0) =>
            new Impure[R, L, B] {
              type X = X0
              val eff = eff0
              val step = step0 |> f
            }
        }
      })
  }

  implicit def unapplyEff[TC[_[_]], R <: Coproduct, L <: Layers[R], A0](
    implicit instance: TC[Eff_[R, L]#O]) = new Unapply[TC, Eff[R, L, A0]] {
    override type A = A0
    override type M[X] = Eff[R, L, X]
    override val TC = instance
    override val leibniz = Leibniz.refl[Eff[R, L, A0]]
  }

  def send[F[_], R <: Coproduct, N[_] <: Coproduct, V](value: F[V])(implicit l: Layers[R] { type O[X] = N[X] }, inj: Inject[N[V], F[V]]): Eff[R, Layers[R] { type O[X] = N[X] }, V] =
    new Impure[R, Layers[R] { type O[X] = N[X] }, V] {
      type X = V
      val eff = Coproduct[N[V]](value)
      val step = Q0[Arr_[R, Layers[R] { type O[X] = N[X] }]#O, V]()
    }

  def qApp[R <: Coproduct, L <: Layers[R], B, W](arrs: Arrs[R, L, B, W]): Arr[R, L, B, W] =
    arrs.tviewl match {
      case e: TAEmptyL[Queue, Arr_[R, L]#O, B, W] => {
        b => Leibniz.symm[Nothing, Any, W, B](e.witness).apply(b).point[Eff_[R, L]#O]
      }
      case kt: :<[Queue, Arr_[R, L]#O, B, _, W] =>
        def bind[A, B](eff: Eff[R, L, A], k: Arrs[R, L, A, B]): Eff[R, L, B] =
          eff.fold(
            qApp(k),
            new Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) => Eff[R, L, B] })#K] {
              override def apply[X0] = {
                (eff0, step0) =>
                  new Impure[R, L, B] {
                    type X = X0
                    val eff = eff0
                    val step = step0 >< k
                  }
              }
            });
        { x => bind(kt.e(x), kt.s) }
    }

  def qcomp[R1 <: Coproduct, L1 <: Layers[R1], R2 <: Coproduct, L2 <: Layers[R2], A, B, C](arrs: Arrs[R1, L1, A, B],
    func: Eff[R1, L1, B] => Eff[R2, L2, C]): Arr[R2, L2, A, C] =
    qApp(arrs) andThen func

  def handleRelay[R1, R <: Coproduct, A, M[_] <: Coproduct, T[_]](
    ret: (A => Eff[R, Layers[R] { type O[X] = M[X] }, A]),
    bind: Forall[({
      type L[V] = (T[V], Arr[R, Layers[R] { type O[X] = M[X] }, V, A]) => Eff[R, Layers[R] { type O[X] = M[X] }, A]
    })#L])(implicit l: Layers.Aux[R, M]): Eff[R1 :+: R, Layers[R1 :+: R] { type O[X] = T[X] :+: M[X] }, A] => Eff[R, Layers[R] { type O[X] = M[X] }, A] =
    _.fold(ret, new Forall[({ type K[X] = (T[X] :+: M[X], Arrs[R1 :+: R, Layers[R1 :+: R] { type O[X] = T[X] :+: M[X] }, X, A]) => Eff[R, Layers[R] { type O[X] = M[X] }, A] })#K] {
      override def apply[X0] = { (eff, step) =>
        val k = qcomp(step, handleRelay[R1, R, A, M, T](ret, bind))
        eff.removeElem[T[X0]] match {
          case Left(x) => bind.apply(x, k)
          case Right(u) =>
            new Impure[R, Layers.Aux[R, M], A] {
              override type X = X0
              override val eff = u
              override val step = Q1[Arr_[R, Layers.Aux[R, M]]#O, X0, A](k)
            }
        }
      }
    })
  def run[A](eff: Eff[CNil, CNil, A]): A = eff.fold(identity, new Forall[({type K[X] = (CNil, Arrs[CNil, CNil, X, A]) => A})#K]{
    override def apply[X]() = {
      (eff, step) =>
      sys.error("This case is only called for CNil, which is supposed to be impossible")
    }
  })
}