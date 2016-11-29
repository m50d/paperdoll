package paperdoll.core.effect

import Predef.identity
import shapeless.{ Coproduct, CNil, :+:, Inl }
import scalaz.{ Monad, Leibniz, Forall, Unapply }
import scalaz.syntax.monad._
import paperdoll.queue.Queue
import paperdoll.core.layer.{ Layer, Layers, Subset }
import scalaz.Functor
import paperdoll.core.nondeterminism.NDet_
import scalaz.MonadPlus
import paperdoll.core.nondeterminism.Nondeterminism
import Arrs.compose

sealed trait Arr_[R <: Coproduct, L <: Layers[R]] {
  final type O[A, B] = A ⇒ Effects[R, L, B]
}

object Arr_ {
  final type One[L <: Layer] = Arr_[L :+: CNil, Layers.One[L]]
  final type Two[L1 <: Layer, L2 <: Layer] = Arr_[L1 :+: L2 :+: CNil, Layers.Two[L1, L2]]
}

object Arrs {
  final type One[L <: Layer, A, B] = Queue[Arr_.One[L]#O, A, B]
  /** Collapse an Arrs (a queue of Arr) to a single Arr.
   */
  def compose[R <: Coproduct, L <: Layers[R], A, B](arrs: Arrs[R, L, A, B]): Arr[R, L, A, B] = {
    value: A ⇒
      arrs.destructureHead.fold({ witness ⇒ Leibniz.symm[Nothing, Any, B, A](witness)(value).point[Effects_[R, L]#O] },
        new Forall[({ type K[W] = (Arr[R, L, A, W], Arrs[R, L, W, B]) ⇒ Effects[R, L, B] })#K] {
          override def apply[W] = {
            (head, tail) ⇒
              val ctail = compose(tail)
              head(value).fold(
                ctail,
                new Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, W]) ⇒ Effects[R, L, B] })#K] {
                  override def apply[X] = (eff, cont) ⇒ Impure[R, L, X, B](eff, cont :+ ctail)
                })
          }
        })
  }
}

/** Intermediate step as a helper for type inference of Effects#extend
 */
final class ExtendingEffects[R <: Coproduct, L <: Layers[R], S <: Coproduct, A](eff: Effects[R, L, A]) {
  def apply[L0 <: Layers[R]]()(implicit su: Subset[S, R] {
    type LT = L0
  }, le: Leibniz[Nothing, Layers[R], L0, L]): Effects[S, su.LS, A] = eff.inject(le.subst[({
    type K[LL] = Subset[S, R] {
      type LT = LL
      type LS = su.LS
    }
  })#K](su))
}

/** A lazy value of type A via a (possibly empty) queue of effects from the list given by R/L
 *  (something like an effectful continuation)
 *  Evaluating this by providing implementations of each effect will eventually yield a value of type A
 */
sealed trait Effects[R <: Coproduct, L <: Layers[R], A] {
  /** This is a "shallow" catamorphism. In practice the main use cases for this are recursive,
   *  folding all the way down, but I found it very difficult to express the required type
   *  for that case.
   *  While calling directly is supported, the most common use case for this is covered by GenericBind.
   */
  def fold[B](pure: A ⇒ B, impure: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) ⇒ B })#K]): B

  private[effect] def inject[S <: Coproduct](implicit su: Subset[S, R] {
    type LT = L
  }): Effects[S, su.LS, A]

  /** Extend this effectful value to one in a larger stack of effects S.
   *  Can also be used to reorder the effect stack (by having S just be a reordering of R)
   */
  final def extend[S <: Coproduct] = new ExtendingEffects[R, L, S, A](this)

  /** Run this effectful value to produce an A. Only available once all effects have been handled
   *  (i.e. R will be CNil) and therefore this Effects must actually be a Pure.
   */
  final def run(implicit l: Leibniz[Nothing, Layers[R], L, Layers[R] { type O[X] = CNil }]): A =
    fold(identity, new Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) ⇒ A })#K] {
      override def apply[X] = (eff, cont) ⇒ l.subst[({ type J[K <: Layers[R]] = K#O[X] })#J](eff).impossible
    })
}
/** An actual A - this is the "nil" case of Effects
 *  Note that this means that a value of type Effects is not necessarily lazy -
 *  Pure(a).map(f) or Pure(a).flatMap(f) will evaluate f(a) immediately (returning Pure(f(a))
 *  or f(a) respectively).
 *  It's only the impure effects that form "suspension points".
 */
final case class Pure[R <: Coproduct, L <: Layers[R], A](a: A) extends Effects[R, L, A] {
  override def fold[B](pure: A ⇒ B, impure: Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) ⇒ B })#K]) = pure(a)
  private[effect] override def inject[S <: Coproduct](implicit su: Subset[S, R] {
    type LT = L
  }): Effects[S, su.LS, A] = Pure(a)
}
/** The "cons" case: an effectful value and a continuation that will eventually lead to an A.
 *  Note that the intermediate type X is hidden from the outside - one is expected
 *  to only access it via the fold method.
 *  While instantiating directly is supported, most use cases should use the simpler Effects#send API
 */
final case class Impure[R <: Coproduct, L <: Layers[R], X, A](eff: L#O[X],
    cont: Arrs[R, L, X, A]) extends Effects[R, L, A] {
  override def fold[B](pure: A ⇒ B, impure: Forall[({ type K[Y] = (L#O[Y], Arrs[R, L, Y, A]) ⇒ B })#K]) =
    impure.apply[X](eff, cont)

  private[effect] override def inject[S <: Coproduct](implicit su: Subset[S, R] { type LT = L }): Effects[S, su.LS, A] =
    Impure[S, su.LS, X, A](su.inject(eff), Queue.one[Arr_[S, su.LS]#O, X, A](compose(cont) andThen { _.inject[S] }))
}

sealed trait Effects_[R <: Coproduct, L <: Layers[R]] {
  final type O[A] = Effects[R, L, A]
}

private[effect] class EffectsMonad[R <: Coproduct, L <: Layers[R]] extends Monad[(Effects_[R, L])#O] {
  override def point[A](a: ⇒ A) = Pure[R, L, A](a)
  override def bind[A, B](fa: Effects[R, L, A])(f: A ⇒ Effects[R, L, B]) =
    fa.fold[Effects[R, L, B]](f, new Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) ⇒ Effects[R, L, B] })#K] {
      override def apply[X] = (eff, cont) ⇒ Impure[R, L, X, B](eff, cont :+ f)
    })
}

/** Lower priority implicit instances for Effects
 */
trait Effects0 {
  /** All Effects form monads (or to be precise: for any fixed stack, the set of possible Effects
   *  for that stack forms a monad), but Effects that include nondeterminism in their effect stack
   *  also form MonadPlus which should be higher priority
   */
  implicit def monadEffects[R <: Coproduct, L <: Layers[R]]: Monad[(Effects_[R, L])#O] =
    new EffectsMonad[R, L]
}

object Effects extends Effects0 {
  /** If the effect stack R includes Nondeterminism_ then the set of possible Effects
   *  for that stack forms a MonadPlus
   */
  implicit def monadPlus[R <: Coproduct, L <: Layers[R], LT0 <: Layers[NDet_ :+: CNil]](
    implicit su: Subset[R, NDet_ :+: CNil] {
      type LS = L
      type LT = LT0
    }, le: Leibniz[Nothing, Layers[NDet_ :+: CNil], LT0, Layers.One[NDet_]]): MonadPlus[Effects_[R, L]#O] =
    new EffectsMonad[R, L] with MonadPlus[Effects_[R, L]#O] {
      override def plus[A](a: Effects[R, L, A], b: ⇒ Effects[R, L, A]) =
        bind(Nondeterminism.Plus.extend[R].apply[LT0]())({
          x ⇒ if (x) a else b
        })
      override def empty[A] = Nondeterminism.Zero[A].extend[R].apply[LT0]()
    }
  /** One[L, A] is the type of an Effects with layer stack just L,
   *  and value type A, i.e. Effects[L :+: CNil, ..., A]
   */
  type One[L <: Layer, A] = Effects[L :+: CNil, Layers.One[L], A]
  sealed trait One_[L <: Layer] {
    final type O[A] = One[L, A]
  }
  type Two[L1 <: Layer, L2 <: Layer, A] = Effects[L1 :+: L2 :+: CNil, Layers.Two[L1, L2], A]
  implicit def unapplyEffects[TC[_[_]], R <: Coproduct, L <: Layers[R], A0](
    implicit instance: TC[Effects_[R, L]#O]) = new Unapply[TC, Effects[R, L, A0]] {
    override type A = A0
    override type M[X] = Effects[R, L, X]
    override val TC = instance
    override val leibniz = Leibniz.refl[Effects[R, L, A0]]
  }
  /** Lift an effectful value L#F[V] to an Effects[L :+: CNil, ..., V].
   *  Usually followed by a .extend to further lift this into a complete effect stack
   */
  def send[L <: Layer, V](value: L#F[V]): Effects.One[L, V] =
    Impure[L :+: CNil, Layers.One[L], V, V](Inl(value), Queue.empty[Arr_[L :+: CNil, Layers.One[L]]#O, V])
  /** Send that infers the types L and V. However since the inference relies on implicit
   *  Functor instances, this will only work if L#F forms a (ScalaZ) Functor
   *  (and the relevant implicit instance is in scope)
   */
  def sendU[FV](value: FV)(implicit u: Unapply[Functor, FV]): Effects.One[Layer.Aux[u.M], u.A] =
    send[Layer.Aux[u.M], u.A](u.leibniz(value))

  /** Send a nested pair of effects F[G[A]], inferring the types based on implicit functor instances.
   */
  def sendTU[FGA, GA](value: FGA)(implicit u1: Unapply[Functor, FGA] {
    type A = GA
  }, u2: Unapply[Functor, GA]): Effects.Two[Layer.Aux[u1.M], Layer.Aux[u2.M], u2.A] = {
    // Inlining this causes compilation to fail, I don't understand why
    def sendGA(ga: GA) = sendU(ga).extend[Layer.Aux[u1.M] :+: Layer.Aux[u2.M] :+: CNil]()
    sendU(value).extend[Layer.Aux[u1.M] :+: Layer.Aux[u2.M] :+: CNil]().flatMap(sendGA(_))
  }

  /** Usually effects can be interleaved, but some effects cannot be expressed
   *  in an interleaveable way (similar to monads which do not have monad transformers).
   *  In that case we may only have one such effect in the stack, and must handle
   *  it last - but we can handle any monadic effect this way.
   */
  def unsafeRun[L <: Layer, A](effects: One[L, A])(implicit monad: Monad[L#F]): L#F[A] =
    effects.fold(monad.point(_),
      new Forall[({ type K[X] = (L#F[X] :+: CNil, Arrs.One[L, X, A]) ⇒ L#F[A] })#K] {
        override def apply[X] = {
          (eff, cont) ⇒
            eff.eliminate(_.flatMap {
              x ⇒ unsafeRun(compose(cont)(x))
            }, _.impossible)
        }
      })
}