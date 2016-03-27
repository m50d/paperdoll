package paperdoll.arm

import resource.Resource
import paperdoll.core.effect.Eff
import resource.ManagedResource
import resource.managed
import scala.reflect.Manifest

sealed trait Region[S, R, A]

private[arm] final case class SHandle[S, R](handle: ManagedResource[R]) extends Region[S, R, R]

object Region {
  def newSHandle[R: Resource: Manifest](r: => R): Eff.One[Region_[S, R], R]#O forSome { type S } =
    Eff.send[Region_[Any, R], R](SHandle(managed(r)))
}