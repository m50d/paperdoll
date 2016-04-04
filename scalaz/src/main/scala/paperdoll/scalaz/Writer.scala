package paperdoll.scalaz

import shapeless.Coproduct
import scalaz.{ Leibniz, Monoid, Writer }
import paperdoll.core.effect.{ Effects, Arr, Bind, Handler }
import paperdoll.core.layer.Layers
import scala.Vector
import scalaz.Leibniz.===
import scalaz.syntax.monad._
import scalaz.syntax.monoid._

object WriterLayer {
  def sendTell[O](o: O): Effects.One[Writer_[O], Unit] =
    Effects.sendU(Writer(o, {}))

  /**
   * Run the writer effect, producing a vector of all the written values
   */
  def runWriterVector[O0]: Handler[Writer_[O0]] {
    type O[X] = (X, Vector[O0])
  } = Effects.handle(new Bind[Writer_[O0]] {
    override type O[X] = (X, Vector[O0])
    override def pure[A](a: A) = (a, Vector())
    override def apply[V, RR <: Coproduct, RL <: Layers[RR], A](writer: Writer[O0, V], arr: Arr[RR, RL, V, (A, Vector[O0])]) =
      {
        val (log, result) = writer.run
        arr(result) map { case (a, l) ⇒ (a, log +: l) }
      }
  })

  /**
   * Run the writer effect, merging all the written values.
   * Notice how we can have multiple interpreters for the same effect,
   * as we've decoupled the declaration of an effect from its implementation.
   */
  def runWriterMonoid[O0: Monoid]: Handler[Writer_[O0]] {
    type O[X] = (X, O0)
  } = Effects.handle(new Bind[Writer_[O0]] {
    override type O[X] = (X, O0)
    override def pure[A](a: A) = (a, Monoid[O0].zero)
    override def apply[V, RR <: Coproduct, RL <: Layers[RR], A](writer: Writer[O0, V], arr: Arr[RR, RL, V, O[A]]) =
      {
        val (log, result) = writer.run
        arr(result) map { case (a, l) ⇒ (a, log |+| l) }
      }
  })
}