package paperdoll.arm

import resource.Resource
import paperdoll.core.effect.Eff

sealed trait Region[S, R, A]

private[arm] case class RegionOperation[S, R, A](op: R => A)(implicit res: Resource[R]) extends Region[S, R, A]

object SHandle {
  def newSHandle[R: Resource](r: R): Eff.One[Region_[S, R], R]#O forSome {type S} =
    Eff.send[Region_[Any, R], R](RegionOperation(identity))
//  def operate
}