package com.github.m50d.paperdoll.layer

import shapeless.{ CNil, Coproduct, :+:, Inl, Inr, HNil, HList, :: }
import scala.annotation.implicitNotFound

/**
 * An effect, represented as a functor-like type F[X]
 * (Note that F does not necessarily need to be a Functor - the machinery
 * will provide suitable map and flatMap operations)
 */
sealed trait Layer {
  type F[X]
}

object Layer {
  type Aux[F0[_]] = Layer {
    type F[X] = F0[X]
  }
}

/**
 * A stack of several possible effects. Each component of R is subtype of Layer
 * TODO: needs a fold-like method that says: if for every component L of R
 * you can handle L#F[X] then you can handle O[X].
 */
@implicitNotFound("${R} is not a stack of layers")
sealed trait Layers[R <: Coproduct] {
  /**
   * The functor-like type of a concrete value for this stack of layers
   */
  type O[X] <: Coproduct
  /**
   * The type of a suitable product of functions to map an O[X] to an A.
   */
  type OHandler[X, A] <: HList
  def handle[X, A](o: O[X], h: OHandler[X, A]): A
}
object Layers {
  /**
   * This is sadly less useful than it might otherwise be because F's kind
   * generally means it has to be expressed as a type lambda, at which point
   * one might as well just define an explicit Layers instead
   */
  type Aux[R <: Coproduct, F[_] <: Coproduct] = Layers[R] {
    type O[X] = F[X]
  }
  implicit def cnil = new Layers[CNil] {
    type O[X] = CNil
    type OHandler[X, A] = HNil
    def handle[X, A](o: CNil, h: HNil) = sys.error("CNil doesn't exist")
  }
  implicit def ccons[H <: Layer, T <: Coproduct](implicit t: Layers[T]) =
    new Layers[H :+: T] {
      type O[X] = H#F[X] :+: t.O[X]
      type OHandler[X, A] = (H#F[X] => A) :: t.OHandler[X, A]
      def handle[X, A](o: H#F[X] :+: t.O[X], h: (H#F[X] => A) :: t.OHandler[X, A]) =
        o match {
          case Inl(x) => h.head(x)
          case Inr(x) => t.handle(x, h.tail)
        }
    }
  def apply[R <: Coproduct](implicit l: Layers[R]): Aux[R, l.O] = l
}