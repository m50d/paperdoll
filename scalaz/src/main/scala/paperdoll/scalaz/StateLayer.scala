package paperdoll.scalaz

import scalaz.State
import paperdoll.core.effect.Effects
import paperdoll.core.effect.Effects.sendU

object StateLayer {
  def sendState[S, A](state: State[S, A]): Effects.One[State_[S], A] =
    sendU(state)
}