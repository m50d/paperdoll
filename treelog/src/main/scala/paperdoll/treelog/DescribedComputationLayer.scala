package paperdoll.treelog

import treelog.LogTreeSyntax
import paperdoll.scalaz.EitherTLayer.sendEitherT
import paperdoll.core.effect.Effects
import paperdoll.scalaz.Writer_
import paperdoll.scalaz.Disjunction_

final case class DescribedComputationLayer[A]() extends LogTreeSyntax[A] {
  def sendDescribedComputation[B](dca: DescribedComputation[B]): Effects.Two[Writer_[LogTree], Disjunction_[String], B] =
    sendEitherT[LogTreeWriter, String, B](dca)
}