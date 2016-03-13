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
  type M[X] <: Coproduct // Layers[S]#O
  type N[X] <: Coproduct // Layers[T]#O
  def inject[X](value: N[X]): M[X]
}
object Subset {
  implicit def nilSubset[S <: Coproduct](implicit l: Layers[S]) = new Subset[S, CNil] {
    override type M[X] = l.O[X]
    override type N[X] = CNil
    override def inject[X](value: CNil) = value.impossible
  }
//  implicit def consSubset[S <: Coproduct, TH <: Layer, TT <: Coproduct](
//    implicit m: Member[S, TH], tl: Subset[S, TT]) =
//    new Subset[S, TH :+: TT] {
//      override type M[X] = tl.M[X]
//      override type N[X] = TH#F[X] :+: tl.N[X]
//      override def inject[X](value: TH#F[X] :+: tl.N[X]) = value match {
//        case Inl(x) ⇒ m.inject(x)
//        case Inr(r) ⇒ tl.inject(r)
//      }
//    }
  //  def apply[S <: Coproduct, M <: Layers[S], T <: Coproduct, N <: Layers[T]](implicit s: Subset[S, M, T, N]) = s
}