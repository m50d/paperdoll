package paperdoll.arm

import resource.Resource
import paperdoll.core.effect.Eff

sealed trait SHandle[S, A]

private[arm] case class Handle[S, R](r: R)(implicit res: Resource[R]) extends SHandle[S, R]

object SHandle {
  def newSHandle[R: Resource](r: R): Eff.One[SHandle_[S], R]#O forSome {type S} =
    Eff.send[SHandle_[String], R](Handle(r))
}