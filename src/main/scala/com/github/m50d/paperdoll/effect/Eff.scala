package com.github.m50d.paperdoll.effect

import shapeless.{ Coproduct, CNil, :+: }
import shapeless.ops.coproduct.Inject
import scalaz.{ Monad, Leibniz, Forall, Unapply }
import scalaz.syntax.monad._
import com.github.m50d.paperdoll.queue.Queue
import com.github.m50d.paperdoll.layer.Layers
import com.github.m50d.paperdoll.layer.Member
import com.github.m50d.paperdoll.layer.Layer

/**
 * A lazy value of type A with a (possibly empty) stack of effects from the list given by R/L
 * (something like an effectful continuation)
 * Evaluating this by providing implementations of each effect will eventually yield a value of type A
 */
sealed trait Eff[R <: Coproduct, L <: Layers[R], A] {
  def fold[B](pure: A => B, impure: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) => B })#K]): B
}
/**
 * An actual A - this is the "nil" case of Eff
 */
private[effect] sealed trait Pure[R <: Coproduct, L <: Layers[R], A] extends Eff[R, L, A] {
  val a: A
  override def fold[B](pure: A => B, impure: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) => B })#K]) = pure(a)
}
/**
 * The "cons" case: an effectful value and a continuation that will eventually lead to an A
 */
private[effect] sealed trait Impure[R <: Coproduct, L <: Layers[R], A] extends Eff[R, L, A] {
  /**
   * Type of the current value
   */
  type X
  /**
   * The current value: an X inside an effect
   */
  val eff: L#O[X]
  /**
   * The continuation from X to what will ultimately be an A
   */
  val cont: Arrs[R, L, X, A]

  override def fold[B](pure: A => B, impure: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) => B })#K]) =
    impure.apply[X](eff, cont)
}
sealed trait Eff_[R <: Coproduct, L <: Layers[R]] {
  final type O[A] = Eff[R, L, A]
}
object Eff {
  implicit def monadEff[R <: Coproduct, L <: Layers[R]] = new Monad[(Eff_[R, L])#O] {
    override def point[A](a0: => A) = new Pure[R, L, A] { val a = a0 }
    override def bind[A, B](fa: Eff[R, L, A])(f: A => Eff[R, L, B]) =
      fa.fold[Eff[R, L, B]](f, new Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) => Eff[R, L, B] })#K] {
        override def apply[X0] = {
          (eff0, cont0) =>
            new Impure[R, L, B] {
              override type X = X0
              override val eff = eff0
              override val cont = cont0 :+ f
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
  /**
   * Lift a single effectful value F[V] into an Eff[F, R, V].
   */
  def send[F[_], R <: Coproduct, N[_] <: Coproduct, V](value: F[V])(
    implicit l: Layers.Aux[R, N], inj: Inject[N[V], F[V]]): Eff[R, Layers.Aux[R, N], V] =
    new Impure[R, Layers[R] { type O[X] = N[X] }, V] {
      override type X = V
      override val eff = Coproduct[N[V]](value)
      override val cont = Queue.empty[Arr_[R, Layers[R] { type O[X] = N[X] }]#O, V]
    }

  /**
   * Collapse an Arrs (a queue of Arr) to a single Arr.
   */
  private[this] def compose[R <: Coproduct, L <: Layers[R], A, B](arrs: Arrs[R, L, A, B]): Arr[R, L, A, B] = {
    value: A =>
      arrs.destructureHead.fold({ witness => Leibniz.symm[Nothing, Any, B, A](witness)(value).point[Eff_[R, L]#O] },
        new Forall[({ type K[W] = (Arr[R, L, A, W], Arrs[R, L, W, B]) => Eff[R, L, B] })#K] {
          override def apply[W] = {
            (head, tail) =>
              head(value).fold(
                compose(tail),
                new Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, W]) => Eff[R, L, B] })#K] {
                  override def apply[X0] = {
                    (eff0, cont0) =>
                      new Impure[R, L, B] {
                        override type X = X0
                        override val eff = eff0
                        override val cont = cont0 ++ tail
                      }
                  }
                })
          }
        })
  }

  /**
   * Handle the effect R1/T, using the supplied handler bind
   * that knows how to translate a T[V] (the concrete effect) and an
   * effectful continuation from V to A with effects R
   * into a single effectful lazy value of type A
   * (usually by somehow "running" the T[V] to obtain a V and then passing it to the continuation)
   * Curried for ease of implementation and to match the Haskell
   */
  def handleRelay[R1 <: Layer, R <: Coproduct, MERR <: Coproduct, MERL <: Layers[MERR], A](
    bind: Forall[({
      type L[V] = (R1#F[V], Arr[MERR, MERL, V, A]) => Eff[MERR, MERL, A]
    })#L])(implicit me: Member[R, R1] {
      type RestR = MERR
      type RestL = MERL
    }): Eff[R, me.L, A] => Eff[MERR, MERL, A] =
    _.fold({ a0 => new Pure[MERR, MERL, A] { val a = a0 } }, new Forall[({ type K[X] = (me.L#O[X], Arrs[R, me.L, X, A]) => Eff[MERR, MERL, A] })#K] {
      override def apply[X0] = { (eff, cont0) =>
        //New continuation is: recursively call handleRelay(bind) on the result of the old continuation 
        val cont1 = compose(cont0) andThen handleRelay[R1, R, MERR, MERL, A](bind)
        me.remove(eff).fold(
          {
            otherEffect =>
              new Impure[MERR, MERL, A] {
                override type X = X0
                override val eff = otherEffect
                override val cont = Queue.one[Arr_[MERR, MERL]#O, X0, A](cont1)
              }
          }, {
            tEffect => bind[X0](tEffect, cont1)
          })
      }
    })
  /**
   * Run a lazy effectful value after all the effects have already been handled -
   * it necessarily no longer contains any actual effects, just the value of type A
   */
  def run[A](eff: Eff[CNil, Layers[CNil] { type O[X] = CNil }, A]): A = eff.fold(identity, new Forall[({ type K[X] = (CNil, Arrs[CNil, Layers[CNil] { type O[X] = CNil }, X, A]) => A })#K] {
    override def apply[X] = {
      (eff, cont) =>
        eff.impossible
    }
  })

  //  def embed[S <: CNil](implicit ls: Layers[S]) = {
  //      def apply[R, L <: Layers[R], A](eff: Eff[R, L, A])(
  //          implicit br: Basis[S, R], bl: Basis[ls.O, L]
  //      ): Eff[S, ls.O]
  //    }
}