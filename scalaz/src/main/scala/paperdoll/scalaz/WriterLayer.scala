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

  def translateWriter[F[_], W](
    implicit mt: MonadTell[F, W]): PureTranslator.Aux[Writer_[W], Layer.Aux[F] :+: CNil, Layers.One[Layer.Aux[F]]] =
    new PureTranslator[Writer_[W]] {
      override type OR = Layer.Aux[F] :+: CNil
      override type OL = Layers.One[Layer.Aux[F]]
      override def run[R <: Coproduct, L1 <: Layers[R], RR <: Coproduct, RL <: Layers[RR], A](eff: Effects[R, L1, A])(
        implicit me: Member[R, Writer_[W]] {
          type L = L1
          type RestR = RR
          type RestL = RL
        },
        su: Subset[RR, OR] {
          type LT = OL
          type LS = RL
        }) = {
        eff.fold(Pure[RR, RL, A](_),
          new Forall[({
            type K[X] = (L1#O[X], Arrs[R, L1, X, A]) ⇒ Effects[RR, RL, A]
          })#K] {
            override def apply[X] = {
              (eff, cont) ⇒
                //New continuation is: recursively run this handler on the result of the old continuation 
                val newCont = compose(cont) andThen { run(_) }
                me.remove(eff).fold(
                  otherEffect ⇒ Impure[RR, RL, X, A](otherEffect, Queue.one[Arr_[me.RestR, me.RestL]#O, X, A](newCont)),
                  { writer ⇒
                    val (log, x) = writer.run
                    sendU(mt.writer(log, x)).extend[RR]().flatMap(newCont)
                  })
            }
          })
      }
    }
}