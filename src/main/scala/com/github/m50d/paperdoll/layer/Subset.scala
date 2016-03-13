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
  implicit def consSubset[S <: Coproduct, TH <: Layer, L0 <: Layers[S], TT <: Coproduct, M0[_] <: Coproduct](
    implicit m: Member[S, TH] { type L = L0 }, tl: Subset[S, TT] { type M[X] = M0[X] }, leib: Leibniz[Nothing, Layers[S], L0, Layers.Aux[S, M0]]) =
    new Subset[S, TH :+: TT] {
      override type M[X] = M0[X]
      override type N[X] = TH#F[X] :+: tl.N[X]
      override def inject[X](value: TH#F[X] :+: tl.N[X]) = value match {
        case Inl(x) ⇒ leib.subst[({ type K[LL] = Member[S, TH] { type L = LL } })#K](m).inject(x)
        case Inr(r) ⇒ tl.inject(r)
      }
    }
    def apply[S <: Coproduct, T <: Coproduct](implicit s: Subset[S, T]) = s
}