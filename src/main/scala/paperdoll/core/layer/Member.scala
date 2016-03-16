package paperdoll.core.layer

import shapeless.{ Coproduct, :+:, Inl, Inr }

/**
 * Typeclass representing that R1 is a member of the layer stack R, and bridging between the
 * layer stack world and the effectful value world.
 * This probably duplicates some functionality that's present in more general form in shapeless.
 * However, if so, I can't understand that general form well enough to express this in terms of it.
 */
sealed trait Member[R <: Coproduct, R1 <: Layer] {
  type L <: Layers[R]
  /**
   * The type R \ R1
   */
  type RestR <: Coproduct
  type RestL <: Layers[RestR]
  def inject[X](value: R1#F[X]): L#O[X]
  def remove[X](value: L#O[X]): Either[RestL#O[X], R1#F[X]]
}

object Member {
  implicit def nil[R1 <: Layer, R <: Coproduct](implicit rest: Layers[R]) = new Member[R1 :+: R, R1] {
    override type L = Layers[R1 :+: R] {
      type O[X] = R1#F[X] :+: rest.O[X]
    }
    override type RestR = R
    override type RestL = Layers.Aux[R, rest.O] // i.e. rest.type
    override def inject[X](value: R1#F[X]) = Inl(value)
    override def remove[X](value: R1#F[X] :+: rest.O[X]) = value match {
      case Inl(x) ⇒ Right(x)
      case Inr(r) ⇒ Left(r)
    }
  }

  implicit def cons[R2 <: Layer, R <: Coproduct, R1 <: Layer](
    implicit rest: Member[R, R1]) =
    new Member[R2 :+: R, R1] {
      override type L = Layers[R2 :+: R] {
        type O[X] = R2#F[X] :+: rest.L#O[X]
      }
      override type RestR = R2 :+: rest.RestR
      override type RestL = Layers[RestR] {
        type O[X] = R2#F[X] :+: rest.RestL#O[X]
      }
      override def inject[X](value: R1#F[X]) = Inr(rest.inject(value))
      override def remove[X](value: R2#F[X] :+: rest.L#O[X]) = value match {
        case Inl(x) ⇒ Left(Inl(x))
        case Inr(r) ⇒ rest.remove(r).left.map(Inr(_))
      }
    }

  def apply[R <: Coproduct, R1 <: Layer](implicit m: Member[R, R1]): Member[R, R1] { type L = m.L; type RestR = m.RestR; type RestL = m.RestL } = m
}