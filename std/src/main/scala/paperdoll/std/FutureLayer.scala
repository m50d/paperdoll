package paperdoll.std

import paperdoll.core.effect.Effects
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import shapeless.{ :+:, CNil }
import scalaz.Forall
import paperdoll.core.effect.Arrs
import scalaz.std.scalaFuture._

object FutureLayer {
  /** We allow running stdlib Future only as the last Layer,
   *  since it's side-effecting and so can't be safely combined with laziness.
   *  For a more pure/composable notion of async,
   *  consider scalaz-concurrent Future instead
   */
  def runFuture[A](effects: Effects.One[Future_, A])(implicit ec: ExecutionContext): Future[A] =
    Effects.handleLast(effects)
}