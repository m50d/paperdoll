package paperdoll.core.effect

import Predef.identity
import shapeless.{ Coproduct, CNil, :+:, Inl }
import scalaz.{ Monad, Leibniz, Forall, Unapply }
import scalaz.syntax.monad._
import paperdoll.core.queue.Queue
import paperdoll.core.layer.{ Layer, Layers, Member, Subset }

sealed trait Arr_[R <: Coproduct, L <: Layers[R]] {
  final type O[A, B] = A ⇒ Eff[R, L, B]
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
   */
  def fold[B](pure: A ⇒ B, impure: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) ⇒ B })#K]): B

  private[effect] def inject[S <: Coproduct](implicit su: Subset[S, R] {
    type LT = L
  }): Eff[S, su.LS, A]

  /**
   * Extend this effectful value to one in a larger stack of effects S.
   * Can also be used to reorder the effect stack
   */
  final def extend[S <: Coproduct] = new ExtendingEff[R, L, S, A](this)
}
/**
 * An actual A - this is the "nil" case of Eff
 */
final case class Pure[R <: Coproduct, L <: Layers[R], A](a: A) extends Eff[R, L, A] {
  override def fold[B](pure: A ⇒ B, impure: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) ⇒ B })#K]) = pure(a)
  override def inject[S <: Coproduct](implicit su: Subset[S, R] {
    type LT = L
  }): Eff[S, su.LS, A] = Pure(a)
}
/**
 * The "cons" case: an effectful value and a continuation that will eventually lead to an A.
 * Note that the intermediate type X is hidden from the outside - one is expected
 * to only access it via the fold method.
 */
final case class Impure[R <: Coproduct, L <: Layers[R], X, A](eff: L#O[X],
  cont: Arrs[R, L, X, A]) extends Eff[R, L, A] {
  override def fold[B](pure: A ⇒ B, impure: Forall[({ type K[Y] = (L#O[Y], Arrs[R, L, Y, A]) ⇒ B })#K]) =
    impure.apply[X](eff, cont)

  override def inject[S <: Coproduct](implicit su: Subset[S, R] { type LT = L }): Eff[S, su.LS, A] =
    Impure[S, su.LS, X, A](su.inject(eff), Queue.one[Arr_[S, su.LS]#O, X, A](Eff.compose(cont) andThen { _.inject[S] }))
}

sealed trait Eff_[R <: Coproduct, L <: Layers[R]] {
  final type O[A] = Eff[R, L, A]
}

/**
 * Handler for the effect L: given an effectful value in a
 * stack of effects R containing L, return the same
 */
sealed trait Handler[L <: Layer] {
  type O[X]
  def apply[R <: Coproduct, L1 <: Layers[R], A, L2 <: Layers[R]](eff: Eff[R, L1, A])(implicit me: Member[R, L] { type L = L2 },
    le: Leibniz[Nothing, Layers[R], L1, L2]): Eff[me.RestR, me.RestL, O[A]]
}
object Handler {
  type Aux[L <: Layer, O0[_]] = Handler[L] {
    type O[X] = O0[X]
  }
}

object Eff {
  /**
   * One[L, A]#O is the type of an Eff with layer stack just L,
   * and value type A, i.e. Eff[L :+: CNil, ..., A]
   */
  sealed trait One[L <: Layer, A] {
    final type O = Eff[L :+: CNil, Layers.One[L]#N, A]
  }
  implicit def monadEff[R <: Coproduct, L <: Layers[R]] = new Monad[(Eff_[R, L])#O] {
    override def point[A](a: ⇒ A) = Pure[R, L, A](a)
    override def bind[A, B](fa: Eff[R, L, A])(f: A ⇒ Eff[R, L, B]) =
      fa.fold[Eff[R, L, B]](f, new Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) ⇒ Eff[R, L, B] })#K] {
        override def apply[X0] =
          (eff0, cont0) ⇒ Impure[R, L, X0, B](eff0, cont0 :+ f)
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
  private[effect] def compose[R <: Coproduct, L <: Layers[R], A, B](arrs: Arrs[R, L, A, B]): Arr[R, L, A, B] = {
    value: A ⇒
      arrs.destructureHead.fold({ witness ⇒ Leibniz.symm[Nothing, Any, B, A](witness)(value).point[Eff_[R, L]#O] },
        new Forall[({ type K[W] = (Arr[R, L, A, W], Arrs[R, L, W, B]) ⇒ Eff[R, L, B] })#K] {
          override def apply[W] = {
            (head, tail) ⇒
              val ctail = compose(tail)
              head(value).fold(
                ctail,
                new Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, W]) ⇒ Eff[R, L, B] })#K] {
                  override def apply[X0] = {
                    (eff, cont) ⇒
                      Impure[R, L, X0, B](eff, cont :+ ctail)
                  }
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
      /**
       * Actual implementation - apply method has extra Leibniz to assist type inference
       */
      private[this] def run[R <: Coproduct, L1 <: Layers[R], A](eff: Eff[R, L1, A])(implicit me: Member[R, L] { type L = L1 }): Eff[me.RestR, me.RestL, O[A]] =
        eff.fold({ a ⇒ Pure[me.RestR, me.RestL, O[A]](bind.pure(a)) }, new Forall[({ type K[X] = (me.L#O[X], Arrs[R, me.L, X, A]) ⇒ Eff[me.RestR, me.RestL, O[A]] })#K] {
          override def apply[X0] = { (eff, cont0) ⇒
            //New continuation is: recursively run this handler on the result of the old continuation 
            val cont1 = compose(cont0) andThen { run(_) }
            me.remove(eff).fold(
              {
                otherEffect ⇒
                  Impure[me.RestR, me.RestL, X0, O[A]](otherEffect, Queue.one[Arr_[me.RestR, me.RestL]#O, X0, O[A]](cont1))
              }, {
                tEffect ⇒ bind[X0, me.RestR, me.RestL, A](tEffect, cont1)
              })
          }
        })
      override def apply[R <: Coproduct, L1 <: Layers[R], A, L2 <: Layers[R]](eff: Eff[R, L1, A])(implicit me: Member[R, L] { type L = L2 },
        le: Leibniz[Nothing, Layers[R], L1, L2]): Eff[me.RestR, me.RestL, O[A]] = run(le.subst[({ type L[X <: Layers[R]] = Eff[R, X, A] })#L](eff))(me)
    }
  /**
   * Run a lazy effectful value after all the effects have already been handled -
   * it necessarily no longer contains any actual effects, just the value of type A
   */
  def run[A](eff: Eff[CNil, Layers[CNil] { type O[X] = CNil }, A]): A = eff.fold(identity, new Forall[({ type K[X] = (CNil, Arrs[CNil, Layers[CNil] { type O[Y] = CNil }, X, A]) ⇒ A })#K] {
    override def apply[X] = {
      (eff, cont) ⇒
        eff.impossible
    }
  })
}