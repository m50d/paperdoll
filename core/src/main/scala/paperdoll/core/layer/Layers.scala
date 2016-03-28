package paperdoll.core.layer

import shapeless.{ CNil, Coproduct, :+: }
import scala.annotation.implicitNotFound

/**
 * Encapsulates an effect, represented as a functor-like type F[X]
 * (Note that F does not necessarily need to be a Functor - the machinery
 * will provide suitable map and flatMap operations).
 * Custom effects should provide their own layer types
 * but these types should never be instantiated, hence sealed
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
 * A stack of several possible effects. Each component of R is subtype of Layer.
 * Often other types contain a type L <: Layers[R]; this should be understood to mean
 * the unique such type L for which an implicit instance exists.
 * The purpose of this type is to map between coproducts of layers R
 * and the type of a concrete value in that stack of layers, O. 
 */
@implicitNotFound("${R} is not a stack of layers")
sealed trait Layers[R <: Coproduct] {
  /**
   * The functor-like type of a concrete value for this stack of layers
   */
  type O[X] <: Coproduct
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
  type Empty = Layers[CNil] {
    type O[X] = CNil
  }
  /**
   * One[L] is the type of a stack consisting of the single layer L
   */
  type One[L <: Layer] = Layers[L :+: CNil] {
    type O[X] = L#F[X] :+: CNil
  }
  implicit def cnil = new Layers[CNil] {
    type O[X] = CNil
  }
  implicit def ccons[H <: Layer, T <: Coproduct](implicit t: Layers[T]) =
    new Layers[H :+: T] {
      type O[X] = H#F[X] :+: t.O[X]
    }
  def apply[R <: Coproduct](implicit l: Layers[R]): Aux[R, l.O] = l
}