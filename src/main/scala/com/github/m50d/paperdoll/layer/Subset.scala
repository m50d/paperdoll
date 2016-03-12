//package com.github.m50d.paperdoll.layer
//
//import shapeless.{ Coproduct, :+:, CNil, Inl, Inr }
//import scalaz.Leibniz
//
///**
// * Typeclass representing that R1 is a member of the layer stack R, and bridging between the
// * layer stack world and the effectful value world.
// * This probably duplicates some functionality that's present in more general form in shapeless.
// * However, if so, I can't understand that general form well enough to express this in terms of it.
// * It also definitely duplicates some functionality from Member
// * but I couldn't get the type inference right without it
// * TODO: Try making L a type parameter of Element (not a type member) for reasons
// */
//sealed trait Element[R <: Coproduct, L <: Layers[R], R1 <: Layer] {
//  def inject[X](value: R1#F[X]): L#O[X]
//}
//
//object Element {
//  implicit def nilElement[R1 <: Layer, R <: Coproduct](implicit rest: Layers[R]) = new Element[R1 :+: R, Layers[R1 :+: R] {
//    type O[X] = R1#F[X] :+: rest.O[X]
//  }, R1] {
//    override def inject[X](value: R1#F[X]) = Inl(value)
//  }
//  implicit def consElement[R2 <: Layer, R <: Coproduct, L <: Layers[R], R1 <: Layer](
//    implicit l: L, rest: Element[R, L, R1]) =
//    new Element[R2 :+: R, Layers[R2 :+: R] {
//      type O[X] = R2#F[X] :+: L#O[X]
//    }, R1] {
//      override def inject[X](value: R1#F[X]) = Inr(rest.inject(value))
//    }
//  def apply[R <: Coproduct, L <: Layers[R], R1 <: Layer](implicit e: Element[R, L, R1]): Element[R, L, R1] = e
//}
//
///**
// * Typeclass representing that the layer stack T is a subset of the layer stack S, and bridging between the
// * layer stack world and the effectful value world.
// * This probably duplicates some functionality that's present in more general form in shapeless.
// * However, if so, I can't understand that general form well enough to express this in terms of it.
// */
//sealed trait Subset[S <: Coproduct, M <: Layers[S], T <: Coproduct, N <: Layers[T]] {
//  def inject[X](value: N#O[X]): M#O[X]
//}
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
////    new Subset[S, TH :+: TT] {
////      override type M = L1
////      override type N = Layers[TH :+: TT] {
////        
////      }
////      
////    }
//  def apply[S <: Coproduct, M <: Layers[S], T <: Coproduct, N <: Layers[T]](implicit s: Subset[S, M, T, N]) = s
//}