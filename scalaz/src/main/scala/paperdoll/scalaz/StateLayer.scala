package paperdoll.scalaz

import scalaz.State
import paperdoll.core.effect.Effects
import paperdoll.core.effect.Effects.sendU
import paperdoll.core.effect.Arrs.compose
import paperdoll.core.effect.{ GenericHandler, Handler }
import shapeless.{CNil, :+:, Coproduct}
import paperdoll.core.layer.Member
import paperdoll.core.effect.Pure
import scalaz.Forall
import paperdoll.core.effect.Arrs
import paperdoll.core.effect.Impure
import paperdoll.core.effect.Arr_
import paperdoll.queue.Queue
import paperdoll.core.effect.GenericSingleTranslator
import scalaz.MonadState
import paperdoll.core.layer.Layer
import paperdoll.core.effect.GenericTranslator
import paperdoll.core.layer.Layers
import scalaz.syntax.monad._

object StateLayer {
  def sendState[S, A](state: State[S, A]): Effects.One[State_[S], A] =
    sendU(state)

  private[this] class StateHandler[S](initialState: S) extends GenericHandler[State_[S]] {
    type O[X] = (S, X)
    override def handler[R <: Coproduct](implicit me1: Member[R, State_[S]]): Handler[R, me1.L, State_[S]] {
      type RestR = me1.RestR
      type RestL = me1.RestL
      type O[X] = StateHandler.this.O[X]
    } = new Handler[R, me1.L, State_[S]] {
      override type RestR = me1.RestR
      override type RestL = me1.RestL
      override type O[X] = StateHandler.this.O[X]
      override def me = me1
      override def run[A](eff: Effects[R, me1.L, A]): Effects[RestR, RestL, (S, A)] =
        eff.fold({ a ⇒ Pure[RestR, RestL, (S, A)]((initialState, a)) },
          new Forall[({ type K[X] = (me1.L#O[X], Arrs[R, me1.L, X, A]) ⇒ Effects[RestR, RestL, (S, A)] })#K] {
            override def apply[X] = { (eff, cont) ⇒
              def newCont(newState: S) = compose(cont) andThen { handleState(newState).handler(me1).run(_) }
              me.remove(eff).fold(
                otherEffect ⇒ Impure[RestR, RestL, X, (S, A)](otherEffect,
                  Queue.One[Arr_[RestR, RestL]#O, X, O[A]](newCont(initialState))),
                { stateEffect ⇒
                  val (newState, result) = stateEffect(initialState)
                  newCont(newState)(result)
                })
            }
          })
    }
  }

  def handleState[S](initialState: S): GenericHandler[State_[S]] {
    type O[A] = (S, A)
  } = new StateHandler(initialState)
  
  def translateState[F[_], S](implicit ms: MonadState[F, S]): GenericTranslator[State_[S]] {
    type OR = Layer.Aux[F] :+: CNil
    type OL = Layers.One[Layer.Aux[F]]
  } = new GenericSingleTranslator[State_[S]] {
    override type O = Layer.Aux[F]
    override def handle[V](eff: State[S, V]) =
      for {
        oldS <- sendU(ms.get)
        (newS, a) = eff.run(oldS)
        _ <- sendU(ms.put(newS))
      } yield a
  }
}