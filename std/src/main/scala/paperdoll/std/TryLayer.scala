package paperdoll.std

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import paperdoll.core.effect.Bind
import paperdoll.core.effect.Handler
import paperdoll.core.effect.Pure
import paperdoll.core.layer.Layers
import shapeless.Coproduct
import paperdoll.core.effect.Arr

object TryLayer {
  /** Try is handled much like Option: if Success,
   *  run the continuation, if Failure, return that.
   */
  def runTry: Handler.Aux[Try_, Try] =
    new Bind[Try_] {
      override type O[X] = Try[X]
      override def pure[A](b: A) = Success(b)
      override def bind[V, RR <: Coproduct, RL <: Layers[RR], A](eff: Try[V], cont: Arr[RR, RL, V, Try[A]]) =
        //Try does not offer a fold-like method so we have to pattern match
        eff match {
          case Success(s) ⇒ cont(s)
          case Failure(f) ⇒ Pure(Failure(f))
        }
    }
}