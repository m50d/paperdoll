package paperdoll.std

import paperdoll.core.effect.Effects
import paperdoll.core.effect.Effects.{send, unsafeRun}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scalaz.std.scalaFuture._

object FutureLayer {
  def sendFuture[A](future: Future[A]): Effects.One[Future_, A] =
    send[Future_, A](future)
    
  /** We can't generally "move future past" other effects -
   *  we could use a callback, but there is no way to guarantee
   *  that that callback is finally run (since other effects
   *  in the stack might include e.g. Option).
   *  So we only allow running Future as the final effect.
   */
  def unsafeRunFuture[A](effects: Effects.One[Future_, A])(implicit ec: ExecutionContext): Future[A] =
    unsafeRun(effects)
}