package paperdoll.scalaz

import paperdoll.core.effect.Arr
import paperdoll.core.effect.PureHandler
import paperdoll.core.effect.Pure
import shapeless.Coproduct
import paperdoll.core.layer.Layers
import paperdoll.core.effect.PureBind
import scalaz.{ -\/, \/-, Disjunction }
import paperdoll.core.effect.Effects.sendU
import paperdoll.core.effect.Effects

object DisjunctionLayer {
  def sendDisjunction[A, B](disjunction: Disjunction[A, B]): Effects.One[Disjunction_[A], B] =
    sendU(disjunction)
  /** Disjunction is handled much like Option: if \/-,
   *  run the continuation, if -\/, return that.
   */
  def handleDisjunction[A]: PureHandler.Aux[Disjunction_[A], Disjunction_[A]#F] =
    new PureBind[Disjunction_[A]] {
      override type O[X] = Disjunction[A, X]
      override def pure[B](b: B) = \/-(b)
      override def bind[V, RR <: Coproduct, RL <: Layers[RR], B](
        eff: Disjunction[A, V], cont: Arr[RR, RL, V, Disjunction[A, B]]) =
        eff.fold(l ⇒ Pure(-\/(l)), cont)
    }
}