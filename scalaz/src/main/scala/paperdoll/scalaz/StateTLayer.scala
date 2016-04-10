package paperdoll.scalaz

import scalaz.{ Functor, State, StateT }
import shapeless.{ :+:, CNil, Inl }
import paperdoll.core.effect.Effects._
import scalaz.Monad
import paperdoll.core.effect.Effects
import paperdoll.core.effect.Impure
import paperdoll.core.layer.Layers
import paperdoll.core.layer.Layer
import paperdoll.core.queue.Queue
import paperdoll.core.effect.Arr_

object StateTLayer {
  def sendStateT[F[_], S, A](stateT: StateT[F, S, A])(implicit monad: Monad[F]): Effects.Two[Layer.Aux[F], State_[S], A] =
    Impure[Layer.Aux[F] :+: State_[S] :+: CNil, Layers.Two[Layer.Aux[F], State_[S]], S ⇒ F[(S, A)], A](
      Inl(stateT.getF[S](monad)), Queue.one[Arr_.Two[Layer.Aux[F], State_[S]]#O, (S ⇒ F[(S, A)]), A]({
        sfsa: (S ⇒ F[(S, A)]) ⇒ (??? : Effects.Two[Layer.Aux[F], State_[S], A])
      }))
}