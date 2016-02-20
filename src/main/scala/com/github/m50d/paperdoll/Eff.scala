package com.github.m50d.paperdoll

import shapeless.{ Coproduct, CNil, :+: }
import shapeless.ops.coproduct.Inject
import scalaz.{ Monad, Leibniz, Forall, Unapply }
import scalaz.syntax.monad._
import com.github.m50d.paperdoll.queue.Queue

/**
 * A lazy value of type A with a (possibly empty) stack of effects from the list given by R/L
 * (something like an effectful continuation)
 * Evaluating this by providing implementations of each effect will eventually yield a value of type A
 */
sealed trait Eff[R <: Coproduct, L <: Layers[R], A] {
  def fold[B](p: A => B, i: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) => B })#K]): B
}
/**
 * An actual A - this is the "nil" case of Eff
 */
sealed trait Pure[R <: Coproduct, L <: Layers[R], A] extends Eff[R, L, A] {
  val a: A
  override def fold[B](p: A => B, i: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) => B })#K]) = p(a)
}
/**
 * The "cons" case: an effectful value and a continuation that will eventually lead to an A
 */
sealed trait Impure[R <: Coproduct, L <: Layers[R], A] extends Eff[R, L, A] {
  /**
   * Type of the present, intermediate value
   */
  type X
  /**
   * The actual value: an X inside an effect
   */
  val eff: L#O[X]
  /**
   * The continuation from X to what will ultimately be an A
   */
  val step: Arrs[R, L, X, A]

  override def fold[B](p: A => B, i: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) => B })#K]) =
    i.apply[X](eff, step)
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
          (eff0, step0) =>
            new Impure[R, L, B] {
              type X = X0
              val eff = eff0
              val step = step0 :+ f
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
      type X = V
      val eff = Coproduct[N[V]](value)
      val step = Queue.empty[Arr_[R, Layers[R] { type O[X] = N[X] }]#O, V]
    }

  /**
   * Collapse an Arrs (a queue of Arr) to a single Arr. Arguably belongs with Queue rather than here
   */
  def qApp[R <: Coproduct, L <: Layers[R], B, W](arrs: Arrs[R, L, B, W]): Arr[R, L, B, W] =
    arrs.destructureHead.fold({ witness => { b: B => Leibniz.symm[Nothing, Any, W, B](witness).apply(b).point[Eff_[R, L]#O] } },
      new Forall[({ type K[V] = (Arr[R, L, B, V], Arrs[R, L, V, W]) => Arr[R, L, B, W] })#K] {
        override def apply[V] = {
          (head, tail) =>
            def bind[A, B](eff: Eff[R, L, A], k: Arrs[R, L, A, B]): Eff[R, L, B] =
              eff.fold(
                qApp(k),
                new Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) => Eff[R, L, B] })#K] {
                  override def apply[X0] = {
                    (eff0, step0) =>
                      new Impure[R, L, B] {
                        type X = X0
                        val eff = eff0
                        val step = step0 ++ k
                      }
                  }
                });
            { x => bind(head(x), tail) }
        }
      })

  /**
   * Compose the function func onto the end of the queue of arrows arrs, resulting in a single Arr
   * with a potentially different effect stack and/or ultimate value.
   */
  private[this] def qcomp[R1 <: Coproduct, L1 <: Layers[R1], R2 <: Coproduct, L2 <: Layers[R2], A, B, C](arrs: Arrs[R1, L1, A, B],
    func: Eff[R1, L1, B] => Eff[R2, L2, C]): Arr[R2, L2, A, C] =
    qApp(arrs) andThen func
  /**
   * Lightly curried - TODO look at whether this could be expressed more clearly.
   * Handle the effect R1/T, using the supplied handler bind
   * that knows how to translate a T[V] (the concrete effect) and an
   * effectful continuation from V to A with effects R/M
   * into a single effectful lazy value of type A
   * (usually by somehow "running" the T[V] to obtain a V and then passing it to the continuation)
   */
  def handleRelay[R1, R <: Coproduct, T[_], M[_] <: Coproduct, A](
    bind: Forall[({
      type L[V] = (T[V], Arr[R, Layers.Aux[R, M], V, A]) => Eff[R, Layers.Aux[R, M], A]
    })#L])(implicit l: Layers.Aux[R, M]): Eff[R1 :+: R, Layers[R1 :+: R] { type O[X] = T[X] :+: M[X] }, A] => Eff[R, Layers.Aux[R, M], A] =
    _.fold({ a0 => new Pure[R, Layers.Aux[R, M], A] { val a = a0 } }, new Forall[({ type K[X] = (T[X] :+: M[X], Arrs[R1 :+: R, Layers[R1 :+: R] { type O[X] = T[X] :+: M[X] }, X, A]) => Eff[R, Layers.Aux[R, M], A] })#K] {
      override def apply[X0] = { (eff, step) =>
        val k = qcomp(step, handleRelay[R1, R, T, M, A](bind))
        eff.removeElem[T[X0]] match {
          case Left(x) => bind.apply(x, k)
          case Right(u) =>
            new Impure[R, Layers.Aux[R, M], A] {
              override type X = X0
              override val eff = u
              override val step = Queue.one[Arr_[R, Layers.Aux[R, M]]#O, X0, A](k)
            }
        }
      }
    })
  /**
   * Run a lazy effectful value after all the effects have already been handled -
   * it necessarily no longer contains any actual effects, just the value of type A
   */
  def run[A](eff: Eff[CNil, Layers[CNil] { type O[X] = CNil }, A]): A = eff.fold(identity, new Forall[({ type K[X] = (CNil, Arrs[CNil, Layers[CNil] { type O[X] = CNil }, X, A]) => A })#K] {
    override def apply[X] = {
      (eff, step) =>
        sys.error("This case is only called for CNil, which is supposed to be impossible")
    }
  })
}