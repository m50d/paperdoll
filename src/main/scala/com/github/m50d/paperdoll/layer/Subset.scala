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
//object Subset {
//  implicit def nilSubset[S <: Coproduct](implicit l: Layers[S]) = new Subset[S, Layers.Aux[S, l.O], CNil, Layers[CNil]{type O[X] = CNil}] {
//    override def inject[X](value: CNil) = value.impossible
//  }
//  implicit def consSubset[S <: Coproduct, M <: Layers[S], TH <: Layer, TT <: Coproduct, NT <: Layers[TT]](
//    implicit e: Element[S, M, TH], tl: Subset[S, M, TT, NT]) =
//      new Subset[S, M, TH :+: TT, Layers[TH :+: TT]{
//        type O[X] = TH#F[X] :+: NT#O[X]
//      }] {
//    override def inject[X](value: TH#F[X] :+: NT#O[X]) = value match {
//        case Inl(x) ⇒ e.inject(x).asInstanceOf
//        case Inr(r) ⇒ tl.inject(r)
//      }
//  }
//  def apply[S <: Coproduct, M <: Layers[S], T <: Coproduct, N <: Layers[T]](implicit s: Subset[S, M, T, N]) = s
//}