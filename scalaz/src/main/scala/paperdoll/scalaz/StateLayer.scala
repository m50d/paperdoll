package paperdoll.scalaz

import scalaz.State
import paperdoll.core.effect.Effects
import paperdoll.core.effect.Effects.sendU
import paperdoll.core.effect.Arrs.compose
import paperdoll.core.effect.{ PureHandler, Handler }
import shapeless.Coproduct
import paperdoll.core.layer.Layers
import paperdoll.core.layer.Member
import paperdoll.core.effect.Pure
import scalaz.Forall
import paperdoll.core.effect.Arrs
import paperdoll.core.effect.Impure
import paperdoll.core.effect.Arr_
import paperdoll.core.queue.Queue

object StateLayer {
  def sendState[S, A](state: State[S, A]): Effects.One[State_[S], A] =
    sendU(state)

  private[this] class StateHandler[S](initialState: S) extends PureHandler[State_[S]] {
    type O[X] = (S, X)
    override def handler[R <: Coproduct, L1 <: Layers[R]](implicit me1: Member[R, State_[S]] {
      type L = L1
    }): Handler[R, L1, State_[S]] {
      type RestR = me1.RestR
      type RestL = me1.RestL
      type O[X] = StateHandler.this.O[X]
    } = new Handler[R, L1, State_[S]] {
      override type RestR = me1.RestR
      override type RestL = me1.RestL
      override type O[X] = StateHandler.this.O[X]
      override def me = me1
      override def run[A](eff: Effects[R, L1, A]): Effects[RestR, RestL, (S, A)] =
        eff.fold({ a ⇒ Pure[RestR, RestL, (S, A)]((initialState, a)) },
          new Forall[({ type K[X] = (L1#O[X], Arrs[R, L1, X, A]) ⇒ Effects[RestR, RestL, (S, A)] })#K] {
            override def apply[X] = { (eff, cont) ⇒
              def newCont(newState: S) = compose(cont) andThen { handleState(newState).handler(me1).run(_) }
              me.remove(eff).fold(
                otherEffect ⇒ Impure[RestR, RestL, X, (S, A)](otherEffect,
                  Queue.one[Arr_[RestR, RestL]#O, X, O[A]](newCont(initialState))),
                { stateEffect ⇒
                  val (newState, result) = stateEffect(initialState)
                  newCont(newState)(result)
                })
            }
          })
    }
  }

  def handleState[S](initialState: S): PureHandler[State_[S]] {
    type O[A] = (S, A)
  } = new StateHandler(initialState)
}