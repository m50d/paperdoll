package paperdoll.core.layer

import shapeless.{ Coproduct, :+:, CNil, Inl, Inr }
import scalaz.Leibniz

/**
 * Typeclass representing that the layer stack T is a subset of the layer stack S, and bridging between the
 * layer stack world and the effectful value world.
 * This probably duplicates some functionality that's present in more general form in shapeless.
 * However, if so, I can't understand that general form well enough to express this in terms of it.
 */
sealed trait Subset[S <: Coproduct, T <: Coproduct] {
  type LS <: Layers[S]
  type LT <: Layers[T]
  def inject[X](value: LT#O[X]): LS#O[X]
}
object Subset {
  implicit def nilSubset[S <: Coproduct](implicit l: Layers[S]) = new Subset[S, CNil] {
    override type LS = Layers.Aux[S, l.O]
    override type LT = Layers[CNil] {
      type O[X] = CNil
    }
    override def inject[X](value: CNil) = value.impossible
  }
  implicit def consSubset[S <: Coproduct, TH <: Layer, L1 <: Layers[_], TT <: Coproduct, L2 <: Layers[_]](
    implicit m: Member[S, TH] { type L = L1 }, tl: Subset[S, TT] { type LS = L2 }, le: Leibniz[Nothing, Any, L1, L2]) =
    new Subset[S, TH :+: TT] {
      override type LS = Layers.Aux[S, L2#O]
      override type LT = Layers[TH :+: TT] {
        type O[X] = TH#F[X] :+: tl.LT#O[X]
      }
      override def inject[X](value: TH#F[X] :+: tl.LT#O[X]) =
        value.eliminate(x ⇒ le.subst[({type K[LL] = Member[S, TH]{type L = LL}})#K](m).inject(x), tl.inject(_))
    }
  def apply[S <: Coproduct, T <: Coproduct](implicit s: Subset[S, T]): Subset[S, T] {type LS = s.LS; type LT = s.LT} = s
}