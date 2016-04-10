package paperdoll.scalaz

import shapeless.Coproduct
import scalaz.{ Monoid, Writer }
import paperdoll.core.effect.{ Effects, Arr, Bind, Handler }
import paperdoll.core.effect.Effects.sendU
import paperdoll.core.layer.Layers
import scalaz.syntax.monad._
import scalaz.syntax.monoid._
import scala.collection.generic.CanBuildFrom

object WriterLayer {
  def sendWriter[W, A](writer: Writer[W, A]): Effects.One[Writer_[W], A] =
    sendU(writer)
  def sendTell[W](w: W): Effects.One[Writer_[W], Unit] =
    sendWriter(Writer(w, {}))

  /**
   * Run the writer effect, producing a collection of all the written values
   */
  def handleWriterCollection[W, CC <: TraversableOnce[W]](implicit cbf: CanBuildFrom[CC, W, CC]): Handler[Writer_[W]] {
    type O[X] = (X, CC)
  } = Effects.handle(new Bind[Writer_[W]] {
    override type O[X] = (X, CC)
    override def pure[A](a: A) = (a, cbf().result)
    override def apply[V, RR <: Coproduct, RL <: Layers[RR], A](writer: Writer[W, V], arr: Arr[RR, RL, V, (A, CC)]) = {
      val (log, result) = writer.run
      arr(result) map { case (a, l) ⇒ (a, cbf() += log ++= l result) }
    }
  })

  /**
   * Run the writer effect, merging all the written values.
   * Notice how we can have multiple interpreters for the same effect,
   * as we've decoupled the declaration of an effect from its implementation.
   */
  def handleWriterMonoid[W: Monoid]: Handler[Writer_[W]] {
    type O[X] = (X, W)
  } = Effects.handle(new Bind[Writer_[W]] {
    override type O[X] = (X, W)
    override def pure[A](a: A) = (a, Monoid[W].zero)
    override def apply[V, RR <: Coproduct, RL <: Layers[RR], A](writer: Writer[W, V], arr: Arr[RR, RL, V, O[A]]) = {
      val (log, result) = writer.run
      arr(result) map { case (a, l) ⇒ (a, log |+| l) }
    }
  })
}