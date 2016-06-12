package paperdoll.scalaz

import shapeless.{ :+:, CNil, Coproduct }
import scalaz.{ Monoid, Writer }
import paperdoll.core.effect.{ Effects, Arr, GenericBind, GenericHandler }
import paperdoll.core.effect.Effects.sendU
import paperdoll.core.layer.Layers
import scalaz.syntax.monad._
import scalaz.syntax.monoid._
import scala.collection.generic.CanBuildFrom
import scalaz.MonadTell
import paperdoll.core.effect.GenericTranslator
import paperdoll.core.layer.Layer
import paperdoll.core.layer.Member
import paperdoll.core.layer.Subset
import paperdoll.core.effect.Handler
import paperdoll.core.effect.Bind
import paperdoll.core.effect.GenericSingleTranslator

object WriterLayer {
  def sendWriter[W, A](writer: Writer[W, A]): Effects.One[Writer_[W], A] =
    sendU(writer)
  def sendTell[W](w: W): Effects.One[Writer_[W], Unit] =
    sendWriter(Writer(w, {}))

  /** Run the writer effect, producing a collection of all the written values
   */
  def handleWriterCollection[W, CC <: TraversableOnce[W]](implicit cbf: CanBuildFrom[CC, W, CC]): GenericHandler[Writer_[W]] {
    type O[X] = (CC, X)
  } = new GenericBind[Writer_[W]] {
    override type O[X] = (CC, X)
    override def pure[A](a: A) = (cbf().result, a)
    override def bind[V, RR <: Coproduct, RL <: Layers[RR], A](writer: Writer[W, V], arr: Arr[RR, RL, V, (CC, A)]) = {
      val (log, result) = writer.run
      arr(result) map { la ⇒ (cbf().+=(log).++=(la._1).result, la._2) }
    }
  }

  /** Run the writer effect, merging all the written values.
   *  Notice how we can have multiple interpreters for the same effect,
   *  as we've decoupled the declaration of an effect from its implementation.
   */
  def handleWriterMonoid[W](implicit monoid: Monoid[W]): GenericHandler[Writer_[W]] {
    type O[X] = (W, X)
  } = new GenericBind[Writer_[W]] {
    override type O[X] = (W, X)
    override def pure[A](a: A) = (monoid.zero, a)
    override def bind[V, RR <: Coproduct, RL <: Layers[RR], A](writer: Writer[W, V], arr: Arr[RR, RL, V, O[A]]) = {
      val (log, result) = writer.run
      arr(result) map { la ⇒ (log |+| la._1, la._2) }
    }
  }

  def translateWriter[F[_], W](implicit mt: MonadTell[F, W]): GenericTranslator[Writer_[W]] {
    type OR = Layer.Aux[F] :+: CNil
    type OL = Layers.One[Layer.Aux[F]]
  } =
    new GenericSingleTranslator[Writer_[W]] {
      override type O = Layer.Aux[F]
      override def handle[V](eff: Writer[W, V]) = {
        val (w, v) = eff.run
        sendU(mt.tell(w)) map { _ ⇒ v }
      }
    }

}