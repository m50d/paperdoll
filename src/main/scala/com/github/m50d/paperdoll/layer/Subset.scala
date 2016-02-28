package com.github.m50d.paperdoll.layer

import shapeless.{Coproduct, :+:, CNil, Inl, Inr}

/**
 * Typeclass representing that the layer stack T is a subset of the layer stack S, and bridging between the
 * layer stack world and the effectful value world.
 * This probably duplicates some functionality that's present in more general form in shapeless.
 * However, if so, I can't understand that general form well enough to express this in terms of it.
 */
sealed trait Subset[S <: Coproduct, T <: Coproduct] {
  type M <: Layers[S]
  type N <: Layers[T]
  def inject[X](value: N#O[X]): M#O[X]
}
object Subset {
  implicit def nil[S <: Coproduct](implicit ls: Layers[S]) = new Subset[S, CNil] {
    override type M = Layers.Aux[S, ls.O]
    override type N = Layers[CNil] {
      type O[X] = CNil
    }
    override def inject[X](value: CNil) = value.impossible
  }
  implicit def cons[S <: Coproduct, TH <: Layer, LS1 <: Layers[S], TT <: Coproduct, LS2 <: Layers[S]](
      implicit me: Member[S, TH], tl: Subset[S, TT]) =
    new Subset[S, TH :+: TT] {
    override type M = tl.M
    override type N = Layers[TH :+: TT] {
      type O[X] = TH#F[X] :+: tl.N#O[X]
    }
    override def inject[X](value: TH#F[X] :+: tl.N#O[X]) = value match {
      case Inl(x) => me.inject(x)
    }
  }
}