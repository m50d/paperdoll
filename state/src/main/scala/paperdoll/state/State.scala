package paperdoll.state

import shapeless.Coproduct
import paperdoll.core.layer.Layers
import paperdoll.core.effect.Eff
import paperdoll.core.layer.Member
import paperdoll.writer.Writer_
import paperdoll.reader.Reader_
import paperdoll.core.effect.Pure
import scalaz.Forall
import paperdoll.core.effect.Arrs
import scalaz.Leibniz
import paperdoll.core.effect.Impure
import paperdoll.core.effect.Arr_
import paperdoll.core.queue.Queue
import paperdoll.core.effect.Arr

final class StateRunner[S] {
  def loop[R <: Coproduct, L0 <: Layers[R], A, RR <: Coproduct, RL <: Layers[RR]](
    eff: Eff[R, L0, A], s: S)(implicit m1: Member[R, Writer_[S]] {
      type L = L0
      type RestR = RR
      type RestL = RL
    }, m2: Member[R, Reader_[S]] {
      type L = RL
    }): Eff[m2.RestR, m2.RestL, (A, S)] = eff.fold[Eff[m2.RestR, m2.RestL, (A, S)]](
    a ⇒ Pure((a, s)),
    new Forall[({ type K[X] = (L0#O[X], Arrs[R, L0, X, A]) ⇒ Eff[m2.RestR, m2.RestL, (A, S)] })#K] {
      override def apply[X] = (eff, cont) ⇒ {
        def newCont(s: S) = Eff.compose(cont) andThen { e ⇒ loop(e, s) }
        m1.remove(eff).fold(
          effr ⇒ m2.remove(effr).fold(
            effrr ⇒ Impure[m2.RestR, m2.RestL, X, (A, S)](effrr, Queue.one[Arr_[m2.RestR, m2.RestL]#O, X, (A, S)](
              newCont(s))),
            _.fold(witness ⇒ Leibniz.symm[Nothing, Any, S, X](witness).subst[({ type K[Y] = Arr[m2.RestR, m2.RestL, Y, (A, S)] })#K](newCont(s))(s))),
          _.fold((newS, witness) ⇒ witness.subst[({type K[Y] = Arr[m2.RestR, m2.RestL, Y, (A, S)]})#K](newCont(newS))({})))
      }
    })

  //  def apply[R <: Coproduct, L0 <: Layers[R], A, L1 <: Layers[R], RR <: Coproduct, RL0 <: Layers[RR], RL1 <: Layers[RR]](
  //    eff: Eff[R, L0, A], s: S)(implicit m1: Member[R, Writer_[S]] {
  //      type L = L1
  //      type RestR = RR
  //      type RestL = RL0
  //    }, m2: Member[R, Reader_[S]] {
  //      type L = RL1
  //    }, l0: Leibniz[Nothing, Layers[R], L0, L1], l1: Leibniz[Nothing, Layers[RR], RL0, RL1]): Eff[m2.RestR, m2.RestL, (A, S)] = 
}

object State {
  def runState[S] = new StateRunner[S]
}