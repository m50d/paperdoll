package paperdoll.scalaz

import scalaz.EitherT
import shapeless.{ :+:, CNil }
import paperdoll.core.effect.Eff
import paperdoll.core.layer.Layer
import paperdoll.core.layer.Layers
import scalaz.Disjunction
import scalaz.syntax.monad._
import paperdoll.core.layer.Subset
import Predef.implicitly
import scalaz.Leibniz

object EitherTLayer {
  /**
   * Inlining this method causes compilation to fail, I don't understand why
   */
  private[this] def sendEx[F[_], A, B](eab: Disjunction[A, B]) =
    Eff.send[Disjunction_[A], B](eab).extend[Layer.Aux[F] :+: Disjunction_[A] :+: CNil]()

  def send[F[_], A, B](et: EitherT[F, A, B]): Eff[Layer.Aux[F] :+: Disjunction_[A] :+: CNil, Layers[Layer.Aux[F] :+: Disjunction_[A] :+: CNil] {
    type O[X] = F[X] :+: Disjunction[A, X] :+: CNil
  }, B] = Eff.send[Layer.Aux[F], Disjunction[A, B]](et.run).extend[Layer.Aux[F] :+: Disjunction_[A] :+: CNil]().flatMap(sendEx[F, A, B])
}