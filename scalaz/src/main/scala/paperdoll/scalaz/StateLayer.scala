package paperdoll.scalaz

import scalaz.State
import paperdoll.core.effect.Effects
import paperdoll.core.effect.Effects.sendU
import paperdoll.core.effect.Arrs.compose
import paperdoll.core.effect.Handler
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

  def handleState[S](initialState: S): Handler[State_[S]] {
    type O[A] = (S, A)
  } = new Handler[State_[S]] {
    type O[A] = (S, A)
    override def run[R <: Coproduct, L1 <: Layers[R], A](eff: Effects[R, L1, A])(
      implicit me: Member[R, State_[S]] { type L = L1 }): Effects[me.RestR, me.RestL, (S, A)] =
      eff.fold({ a ⇒ Pure[me.RestR, me.RestL, (S, A)]((initialState, a)) },
        new Forall[({ type K[X] = (me.L#O[X], Arrs[R, me.L, X, A]) ⇒ Effects[me.RestR, me.RestL, (S, A)] })#K] {
          override def apply[X] = { (eff, cont) ⇒
            def newCont(newState: S) = compose(cont) andThen { handleState(newState).run(_) }
            me.remove(eff).fold(
              otherEffect ⇒ Impure[me.RestR, me.RestL, X, (S, A)](otherEffect,
                Queue.one[Arr_[me.RestR, me.RestL]#O, X, O[A]](newCont(initialState))),
              { stateEffect ⇒
                val (newState, result) = stateEffect(initialState)
                newCont(newState)(result)
              })
          }
        })
  }
}