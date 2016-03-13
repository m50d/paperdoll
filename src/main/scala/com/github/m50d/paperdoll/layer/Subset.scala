package com.github.m50d.paperdoll.layer

import shapeless.{ Coproduct, :+:, CNil, Inl, Inr }
import scalaz.Leibniz

/**
 * Typeclass representing that the layer stack T is a subset of the layer stack S, and bridging between the
 * layer stack world and the effectful value world.
 * This probably duplicates some functionality that's present in more general form in shapeless.
 * However, if so, I can't understand that general form well enough to express this in terms of it.
 */
sealed trait Subset[S <: Coproduct, T <: Coproduct] {
  type LT <: Layers[T]
  type O[X] <: Coproduct // Layers[S]#O
  def inject[X](value: LT#O[X]): O[X]
}
object Subset {
  implicit def nilSubset[S <: Coproduct](implicit l: Layers[S]) = new Subset[S, CNil] {
    override type LT = Layers[CNil] {
      type O[X] = CNil
    }
    override type O[X] = l.O[X]
    override def inject[X](value: CNil) = value.impossible
  }
  implicit def consSubset[S <: Coproduct, TH <: Layer, TT <: Coproduct](
    implicit m: Member[S, TH], tl: Subset[S, TT]) =
    new Subset[S, TH :+: TT] {
      override type LT = Layers[TH :+: TT] {
        type O[X] = TH#F[X] :+: tl.LT#O[X]
      }
      override type O[X] = tl.O[X]
      override def inject[X](value: TH#F[X] :+: tl.LT#O[X]) = value match {
        case Inl(x) ⇒ m.inject(x).asInstanceOf //TODO
        case Inr(r) ⇒ tl.inject(r)
      }
    }
  def apply[S <: Coproduct, T <: Coproduct](implicit s: Subset[S, T]) = s
}