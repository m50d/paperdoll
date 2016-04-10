package paperdoll.scalaz

import scalaz.{Functor, State, StateT}
import paperdoll.core.effect.Effects._
import scalaz.Monad

object StateTLayer {
  def sendStateT[F[_], S, A](stateT: StateT[F, S, A])(implicit monad: Monad[F]) = 
     sendU(stateT.getF[S](monad)) //TODO the rest
}