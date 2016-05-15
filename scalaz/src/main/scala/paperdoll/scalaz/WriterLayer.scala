package paperdoll.scalaz

import shapeless.{ :+:, CNil, Coproduct }
import scalaz.{ Monoid, Writer }
import paperdoll.core.effect.{ Effects, Arr, PureBind, PureHandler }
import paperdoll.core.effect.Effects.sendU
import paperdoll.core.effect.Arrs.compose
import paperdoll.core.layer.Layers
import scalaz.syntax.monad._
import scalaz.syntax.monoid._
import scala.collection.generic.CanBuildFrom
import scalaz.MonadTell
import paperdoll.core.effect.PureTranslator
import paperdoll.core.layer.Layer
import paperdoll.core.layer.Member
import paperdoll.core.layer.Subset
import paperdoll.core.effect.Pure
import scalaz.Forall
import paperdoll.core.effect.Arrs
import paperdoll.core.effect.Arr_
import paperdoll.core.effect.Impure
import paperdoll.core.queue.Queue
import paperdoll.core.effect.Loop
import paperdoll.core.effect.Handler

object WriterLayer {
  def sendWriter[W, A](writer: Writer[W, A]): Effects.One[Writer_[W], A] =
    sendU(writer)
  def sendTell[W](w: W): Effects.One[Writer_[W], Unit] =
    sendWriter(Writer(w, {}))

  /** Run the writer effect, producing a collection of all the written values
   */
  def handleWriterCollection[W, CC <: TraversableOnce[W]](implicit cbf: CanBuildFrom[CC, W, CC]): PureHandler[Writer_[W]] {
    type O[X] = (CC, X)
  } = new PureBind[Writer_[W]] {
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
  def handleWriterMonoid[W](implicit monoid: Monoid[W]): PureHandler[Writer_[W]] {
    type O[X] = (W, X)
  } = new PureBind[Writer_[W]] {
    override type O[X] = (W, X)
    override def pure[A](a: A) = (monoid.zero, a)
    override def bind[V, RR <: Coproduct, RL <: Layers[RR], A](writer: Writer[W, V], arr: Arr[RR, RL, V, O[A]]) = {
      val (log, result) = writer.run
      arr(result) map { la ⇒ (log |+| la._1, la._2) }
    }
  }

  def translateWriter[F[_], W](implicit mt: MonadTell[F, W]): PureTranslator[Writer_[W]]{
    type OR= Layer.Aux[F] :+: CNil
    type OL = Layers.One[Layer.Aux[F]]
  } =
    new PureTranslator[Writer_[W]] {
    override type OR = Layer.Aux[F] :+: CNil
    override type OL = Layers.One[Layer.Aux[F]]
    override def handler[R <: Coproduct, L1 <: Layers[R], RR <: Coproduct, RL <: Layers[RR]](
        implicit me1: Member[R, Writer_[W]]{type L = L1; type RestR = RR; type RestL = RL},
        su: Subset[RR, OR]{type LT = OL; type LS = RL}): Handler[R,L1, Writer_[W]]{type RestR = RR; type RestL = RL; type O[X] = X} =
        new Loop[R, L1, Writer_[W]] {
          override type RestR = RR
          override type RestL = RL
          override type O[X] = X
          override def me = me1
          override def pure[A](a: A) = a
          override def bind[V, A](eff: Writer[W, V], cont: V ⇒ Effects[RR, RL, A]): Effects[RR, RL, A] = {
            val (w, v) = eff.run
            sendU(mt.tell(w)).extend[RR]() flatMap { _ ⇒ cont(v) }
          }
        }
  }

}