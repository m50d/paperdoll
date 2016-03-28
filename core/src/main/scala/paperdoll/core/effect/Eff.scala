package paperdoll.core.effect

import Predef.identity
import shapeless.{ Coproduct, CNil, :+:, Inl }
import scalaz.{ Monad, Leibniz, Forall, Unapply }
import scalaz.syntax.monad._
import paperdoll.core.queue.Queue
import paperdoll.core.layer.{ Layer, Layers, Member, Subset }

sealed trait Arr_[R <: Coproduct, L <: Layers[R]] {
  final type O[A, B] = A ⇁EEff[R, L, B]
}

/**
 * Intermediate step as a helper for type inference of Eff#extend
 */
final class ExtendingEff[R <: Coproduct, L <: Layers[R], S <: Coproduct, A](eff: Eff[R, L, A]) {
  def apply[L0 <: Layers[R]]()(implicit su: Subset[S, R] {
    type LT = L0
  }, le: Leibniz[Nothing, Layers[R], L0, L]): Eff[S, su.LS, A] = eff.inject(le.subst[({
    type K[LL] = Subset[S, R] {
      type LT = LL
      type LS = su.LS
    }
  })#K](su))
}

/**
 * A lazy value of type A with a (possibly empty) stack of effects from the list given by R/L
 * (something like an effectful continuation)
 * Evaluating this by providing implementations of each effect will eventually yield a value of type A
 */
sealed trait Eff[R <: Coproduct, L <: Layers[R], A] {
  /**
   * This is a "shallow" cata. In practice the main use cases for this are recursive,
   * folding all the way down, but I found it very difficult to express the required type
   * for that case.
   * While calling directly is supported, the most common use case for this is covered by Eff#handle.
   */
  def fold[B](pure: A ⇁EB, impure: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) ⇁EB })#K]): B

  private[effect] def inject[S <: Coproduct](implicit su: Subset[S, R] {
    type LT = L
  }): Eff[S, su.LS, A]

  /**
   * Extend this effectful value to one in a larger stack of effects S.
   * Can also be used to reorder the effect stack
   */
  final def extend[S <: Coproduct] = new ExtendingEff[R, L, S, A](this)

  /**
   * Run this effectful value to produce an A. Only available once all effects have been handled
   * (i.e. R will be CNil) and therefore this Eff must actually be a Pure.
   */
  final def run(implicit l: Leibniz[Nothing, Layers[R], L, Layers[R] { type O[X] = CNil }]): A =
    fold(identity, new Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) ⇁EA })#K] {
      override def apply[X] = (eff, cont) ⇁El.subst[({ type J[K <: Layers[R]] = K#O[X] })#J](eff).impossible
    })
}
/**
 * An actual A - this is the "nil" case of Eff
 */
final case class Pure[R <: Coproduct, L <: Layers[R], A](a: A) extends Eff[R, L, A] {
  override def fold[B](pure: A ⇁EB, impure: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) ⇁EB })#K]) = pure(a)
  override def inject[S <: Coproduct](implicit su: Subset[S, R] {
    type LT = L
  }): Eff[S, su.LS, A] = Pure(a)
}
/**
 * The "cons" case: an effectful value and a continuation that will eventually lead to an A.
 * Note that the intermediate type X is hidden from the outside - one is expected
 * to only access it via the fold method.
 * While instantiating directly is supported, most use cases should use the simpler Eff#send API
 */
final case class Impure[R <: Coproduct, L <: Layers[R], X, A](eff: L#O[X],
  cont: Arrs[R, L, X, A]) extends Eff[R, L, A] {
  override def fold[B](pure: A ⇁EB, impure: Forall[({ type K[Y] = (L#O[Y], Arrs[R, L, Y, A]) ⇁EB })#K]) =
    impure.apply[X](eff, cont)

  override def inject[S <: Coproduct](implicit su: Subset[S, R] { type LT = L }): Eff[S, su.LS, A] =
    Impure[S, su.LS, X, A](su.inject(eff), Queue.one[Arr_[S, su.LS]#O, X, A](Eff.compose(cont) andThen { _.inject[S] }))
}

sealed trait Eff_[R <: Coproduct, L <: Layers[R]] {
  final type O[A] = Eff[R, L, A]
}

object Eff {
  /**
   * One[L, A] is the type of an Eff with layer stack just L,
   * and value type A, i.e. Eff[L :+: CNil, ..., A]
   */
  type One[L <: Layer, A] = Eff[L :+: CNil, Layers.One[L], A]
  sealed trait One_[L <: Layer] {
    final type O[A] = One[L, A]#O
  }
  implicit def monadEff[R <: Coproduct, L <: Layers[R]] = new Monad[(Eff_[R, L])#O] {
    override def point[A](a: ⇁EA) = Pure[R, L, A](a)
    override def bind[A, B](fa: Eff[R, L, A])(f: A ⇁EEff[R, L, B]) =
      fa.fold[Eff[R, L, B]](f, new Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) ⇁EEff[R, L, B] })#K] {
        override def apply[X] = (eff, cont) ⇁EImpure[R, L, X, B](eff, cont :+ f)
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
   * Lift an effectful value L#F[V] to an Eff[L :+: CNil, ..., V].
   * Usually followed by a .extend to further lift this into a complete effect stack
   */
  def send[L <: Layer, V](value: L#F[V]): Eff.One[L, V]#O =
    Impure[L :+: CNil, Layers.One[L]#N, V, V](Inl(value), Queue.empty[Arr_[L :+: CNil, Layers.One[L]#N]#O, V])
  /**
   * Collapse an Arrs (a queue of Arr) to a single Arr.
   */
  def compose[R <: Coproduct, L <: Layers[R], A, B](arrs: Arrs[R, L, A, B]): Arr[R, L, A, B] = {
    value: A ⇁E
      arrs.destructureHead.fold({ witness ⇁ELeibniz.symm[Nothing, Any, B, A](witness)(value).point[Eff_[R, L]#O] },
        new Forall[({ type K[W] = (Arr[R, L, A, W], Arrs[R, L, W, B]) ⇁EEff[R, L, B] })#K] {
          override def apply[W] = {
            (head, tail) ⇁E
              val ctail = compose(tail)
              head(value).fold(
                ctail,
                new Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, W]) ⇁EEff[R, L, B] })#K] {
                  override def apply[X] = (eff, cont) ⇁EImpure[R, L, X, B](eff, cont :+ ctail)
                })
          }
        })
  }
  /**
   * "Lift" bind to produce a Handler for L.
   */
  def handle[L <: Layer](bind: Bind[L]): Handler.Aux[L, bind.O] =
    new Handler[L] {
      override type O[X] = bind.O[X]
      override def run[R <: Coproduct, L1 <: Layers[R], A](eff: Eff[R, L1, A])(implicit me: Member[R, L] { type L = L1 }): Eff[me.RestR, me.RestL, O[A]] =
        eff.fold({ a ⇁EPure[me.RestR, me.RestL, O[A]](bind.pure(a)) }, new Forall[({ type K[X] = (me.L#O[X], Arrs[R, me.L, X, A]) ⇁EEff[me.RestR, me.RestL, O[A]] })#K] {
          override def apply[X] = { (eff, cont) ⇁E
            //New continuation is: recursively run this handler on the result of the old continuation 
            val newCont = compose(cont) andThen { run(_) }
            me.remove(eff).fold(
              otherEffect ⇁EImpure[me.RestR, me.RestL, X, O[A]](otherEffect, Queue.one[Arr_[me.RestR, me.RestL]#O, X, O[A]](newCont)),
              thisEffect ⇁Ebind[X, me.RestR, me.RestL, A](thisEffect, newCont))
          }
        })
    }
}