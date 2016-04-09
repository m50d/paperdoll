package paperdoll.scalaz

import paperdoll.core.effect.Arr
import paperdoll.core.effect.Handler
import paperdoll.core.effect.Pure
import shapeless.Coproduct
import paperdoll.core.layer.Layers
import paperdoll.core.effect.Bind
import scalaz.{-\/, \/-, Disjunction}
import paperdoll.core.effect.Effects.handle

object DisjunctionLayer {
    /** Disjunction is handled much like Option: if \/-,
   *  run the continuation, if -\/, return that.
   */
  def handleDisjunction[A]: Handler.Aux[Disjunction_[A], Disjunction_[A]#F] =
    handle(new Bind[Disjunction_[A]] {
      override type O[X] = Disjunction[A, X]
      override def pure[B](b: B) = \/-(b)
      override def apply[V, RR <: Coproduct, RL <: Layers[RR], B](
          eff: Disjunction[A, V], cont: Arr[RR, RL, V, Disjunction[A, B]]) =
        eff.fold(l ⇒ Pure(-\/(l)), cont)
    })
}