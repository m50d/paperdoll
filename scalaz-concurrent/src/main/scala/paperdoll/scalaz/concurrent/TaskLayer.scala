package paperdoll.scalaz.concurrent

import paperdoll.core.effect.Effects.{sendU, unsafeRun}
import scalaz.concurrent.Task
import paperdoll.core.effect.Effects

object TaskLayer {
  def sendTask[A](fa: Task[A]): Effects.One[Task_, A] = sendU(fa)
  /** We can't generally "move task past" other effects -
   *  we could use a callback, but there is no way to guarantee
   *  that that callback is finally run (since other effects
   *  in the stack might include e.g. Option).
   *  So we only allow running Task as the final effect.
   */
  def unsafeRunTask[A](effects: Effects.One[Task_, A]): Task[A] =
    unsafeRun(effects)
}