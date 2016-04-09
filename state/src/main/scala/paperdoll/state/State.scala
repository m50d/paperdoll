package paperdoll.state

import shapeless.Coproduct
import paperdoll.core.layer.Layers
import paperdoll.core.effect.Effects
import paperdoll.core.layer.Member
import paperdoll.scalaz.Writer_
import paperdoll.scalaz.Reader_
import paperdoll.core.effect.Pure
import scalaz.Forall
import paperdoll.core.effect.Arrs
import scalaz.Leibniz
import paperdoll.core.effect.Impure
import paperdoll.core.effect.Arr_
import paperdoll.core.queue.Queue
import paperdoll.core.effect.Arr

final class StateRunner[S] {
  def loop[R <: Coproduct, L0 <: Layers[R], A, RR <: Coproduct, RL0 <: Layers[RR], RL1 <: Layers[_]](
    eff: Effects[R, L0, A], s: S)(implicit m0: Member[R, Writer_[S]] {
      type L = L0
      type RestR = RR
      type RestL = RL0
    }, m1: Member[RR, Reader_[S]] {
      type L = RL1
    }, l: Leibniz[Nothing, Layers[_], RL0, RL1]): Effects[m1.RestR, m1.RestL, (A, S)] = eff.fold[Effects[m1.RestR, m1.RestL, (A, S)]](
    a ⇒ Pure((a, s)),
    new Forall[({ type K[X] = (L0#O[X], Arrs[R, L0, X, A]) ⇒ Effects[m1.RestR, m1.RestL, (A, S)] })#K] {
      override def apply[X] = (eff, cont) ⇒ {
        def newCont(s: S) = Effects.compose(cont) andThen { e ⇒ loop(e, s) }
        m0.remove(eff).fold(
          effr ⇒ m1.remove(l.subst[({ type K[RLL <: Layers[_]] = RLL#O[X] })#K](effr)).fold(
            effrr ⇒ Impure[m1.RestR, m1.RestL, X, (A, S)](effrr, Queue.one[Arr_[m1.RestR, m1.RestL]#O, X, (A, S)](
              newCont(s))),
            r => newCont(s)(r(s))),
          { effw =>
            val (newS, v) = effw.run
            newCont(newS)(v)
          })
      }
    })

  def apply[R <: Coproduct, L0 <: Layers[R], A, L1 <: Layers[R], RR <: Coproduct, RL0 <: Layers[RR], RL1 <: Layers[_]](
    eff: Effects[R, L0, A], s: S)(implicit m0: Member[R, Writer_[S]] {
      type L = L1
      type RestR = RR
      type RestL = RL0
    }, m1: Member[RR, Reader_[S]] {
      type L = RL1
    }, l0: Leibniz[Nothing, Layers[R], L1, L0], l1: Leibniz[Nothing, Layers[_], RL0, RL1]): Effects[m1.RestR, m1.RestL, (A, S)] =
    loop[R, L0, A, RR, RL0, RL1](eff, s)(l0.subst[({
      type K[LL <: Layers[R]] = Member[R, Writer_[S]] {
        type L = LL
        type RestR = RR
        type RestL = RL0
      }
    })#K](m0), m1, l1)
}

object State {
  def runState[S] = new StateRunner[S]
}