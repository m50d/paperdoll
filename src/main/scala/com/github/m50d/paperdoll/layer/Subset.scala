package com.github.m50d.paperdoll.layer

import shapeless.{ Coproduct, :+:, CNil, Inl, Inr }
import scalaz.Leibniz

/**
 * Typeclass representing that R1 is a member of the layer stack R, and bridging between the
 * layer stack world and the effectful value world.
 * This probably duplicates some functionality that's present in more general form in shapeless.
 * However, if so, I can't understand that general form well enough to express this in terms of it.
 * It also definitely duplicates some functionality from Member
 * but I couldn't get the type inference right without it
 */
sealed trait Element[R <: Coproduct, R1 <: Layer] {
  type L <: Layers[R]
  def inject[X](value: R1#F[X]): L#O[X]
}

trait Element1 {
  implicit def nil[R1 <: Layer, R <: Coproduct](implicit rest: Layers[R]) = new Element[R1 :+: R, R1] {
    override type L = Layers[R1 :+: R] {
      type O[X] = R1#F[X] :+: rest.O[X]
    }
    override def inject[X](value: R1#F[X]) = Inl(value)
  }
}

object Element extends Element1 {
  implicit def cons[R2 <: Layer, R <: Coproduct, R1 <: Layer](
    implicit rest: Element[R, R1]) =
    new Element[R2 :+: R, R1] {
      override type L = Layers[R2 :+: R] {
        type O[X] = R2#F[X] :+: rest.L#O[X]
      }
      override def inject[X](value: R1#F[X]) = Inr(rest.inject(value))
    }
  def apply[R <: Coproduct, R1 <: Layer](implicit e: Element[R, R1]): Element[R, R1] { type L = e.L } = e
}

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
  implicit def cons[S <: Coproduct, TH <: Layer, TT <: Coproduct](
    implicit sh: SubsetHelper[S, TH, TT]) =
    new Subset[S, TH :+: TT] {
      override type M = sh.J
      override type N = Layers[TH :+: TT] {
        type O[X] = TH#F[X] :+: sh.LTT#O[X]
      }
      override def inject[X](value: TH#F[X] :+: sh.LTT#O[X]) = value match {
        case Inl(x) ⇒ sh.e.inject(x)
        case Inr(r) ⇒ sh.tl.inject(r)
      }
    }
  def apply[S <: Coproduct, T <: Coproduct](implicit s: Subset[S, T]): Subset[S, T] {
    type M = s.M
    type N = s.N
  } = s
}
sealed trait SubsetHelper[S <: Coproduct, TH <: Layer, TT <: Coproduct] {
  type J <: Layers[S]
  type LTT <: Layers[TT]
  val e: Element[S, TH] { type L = J }
  val tl: Subset[S, TT] { type M = J; type N = LTT }
}
object SubsetHelper {
  implicit def help[S <: Coproduct, TH <: Layer, TT <: Coproduct, L1 <: Layers[S], L2 <: Layers[S]](
    implicit e0: Element[S, TH] { type L = L1 }, tl0: Subset[S, TT] { type M = L2 }, le: Leibniz[Nothing, Layers[S], L1, L2]) =
    new SubsetHelper[S, TH, TT] {
      type J = L2
      type LTT = tl0.N
      val e = le.subst[({ type K[Y] = Element[S, TH] { type L = Y } })#K](e0)
      val tl = tl0: Subset[S, TT] {
        type M = L2
        type N = tl0.N
      }
    }
}