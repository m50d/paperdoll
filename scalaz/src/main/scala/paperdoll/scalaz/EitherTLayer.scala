package paperdoll.scalaz

import scalaz.EitherT
import shapeless.{ :+:, CNil }
import paperdoll.core.effect.Eff
import paperdoll.core.layer.Layer
import paperdoll.core.layer.Layers
import scalaz.Disjunction
import scalaz.syntax.monad._
import paperdoll.core.layer.Subset
import paperdoll.core.layer.Member
import Predef.implicitly
import scalaz.Leibniz

object EitherTLayer {
  def send[F[_], A, B](et: EitherT[F, A, B]): Eff[Layer.Aux[F] :+: Disjunction_[A] :+: CNil, Layers[Layer.Aux[F] :+: Disjunction_[A] :+: CNil] {
    type O[X] = F[X] :+: Disjunction[A, X] :+: CNil
  }, B] = Eff.send[Layer.Aux[F], Disjunction[A, B]](et.run).extend[Layer.Aux[F] :+: Disjunction_[A] :+: CNil]().flatMap {
    eab =>
      val eff = Eff.send[Disjunction_[A], B](eab)
      eff.extend[Layer.Aux[F] :+: Disjunction_[A] :+: CNil]()(
          Subset[Layer.Aux[F] :+: Disjunction_[A] :+: CNil, Disjunction_[A] :+: CNil],
          Leibniz.refl[Layers.One[Disjunction_[A]]])
  }
}