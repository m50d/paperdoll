package paperdoll.nondeterminism

import scalaz.Leibniz.===
import paperdoll.core.layer.Layers
import shapeless.Coproduct
import scalaz.MonadPlus
import paperdoll.core.effect.Eff_
import paperdoll.core.effect.Impure
import paperdoll.core.effect.Eff
import paperdoll.core.effect.Pure
import scalaz.Forall
import paperdoll.core.effect.Arrs
import scalaz.Leibniz
import shapeless.{ :+:, CNil }
import paperdoll.core.layer.Subset
import scalaz.syntax.foldable._
import scalaz.std.list._
import scalaz.syntax.std.list._
import paperdoll.core.layer.Member
import paperdoll.core.effect.Arr_
import paperdoll.core.queue.Queue
import paperdoll.core.effect.Arr
import scalaz.syntax.monad._

sealed trait NDet[A] {
  def fold[B](zero: ⇒ B, plus: A === Boolean ⇒ B): B
}

object NDet {
  private[this] def zero[A] = new NDet[A] {
    override def fold[B](zero: ⇒ B, plus: A === Boolean ⇒ B) = zero
  }
  private[this] def plus = new NDet[Boolean] {
    override def fold[B](zero: ⇒ B, plus: Boolean === Boolean ⇒ B) = plus(Leibniz.refl)
  }
  def Zero[A] = Eff.send[NDet_, A](zero)
  def Plus = Eff.send[NDet_, Boolean](plus)

  //TODO remove duplication between this and the other case
  implicit def monadPlus[R <: Coproduct, L <: Layers[R], LT0 <: Layers[NDet_ :+: CNil]](implicit su: Subset[R, NDet_ :+: CNil] {
    type LS = L
    type LT = LT0
  }, le: Leibniz[Nothing, Layers[NDet_ :+: CNil], LT0, Layers.One[NDet_]]): MonadPlus[Eff_[R, L]#O] =
    new MonadPlus[Eff_[R, L]#O] {
      override def point[A](a: ⇒ A) = Pure[R, L, A](a)
      override def bind[A, B](fa: Eff[R, L, A])(f: A ⇒ Eff[R, L, B]) =
        fa.fold[Eff[R, L, B]](f, new Forall[({ type K[X] = (L#O[X], Arrs[R, L, X, A]) ⇒ Eff[R, L, B] })#K] {
          override def apply[X] = (eff, cont) ⇒ Impure[R, L, X, B](eff, cont :+ f)
        })
      override def plus[A](a: Eff[R, L, A], b: ⇒ Eff[R, L, A]) =
        bind(Eff.send[NDet_, Boolean](NDet.this.plus).extend[R].apply[LT0]())({
          x ⇒ if (x) a else b
        })
      override def empty[A] = Eff.send[NDet_, A](zero).extend[R].apply[LT0]()
    }

  private[this] def loop[R <: Coproduct, L0 <: Layers[R], A](
    jq: List[Eff[R, L0, A]], j: Eff[R, L0, A])(implicit su: Subset[R, NDet_ :+: CNil] {
      type LS = L0
      type LT = Layers.One[NDet_]
    }, me: Member[R, NDet_] { type L = L0 }): Eff[R, L0, Option[(A, Eff[R, L0, A])]] =
    j.fold(a => Pure[R, L0, Option[(A, Eff[R, L0, A])]](Some((a, jq.msuml[Eff_[R, L0]#O, A]))),
      new Forall[({ type K[X] = (L0#O[X], Arrs[R, L0, X, A]) => Eff[R, L0, Option[(A, Eff[R, L0, A])]] })#K] {
        override def apply[X] = {
          (eff, cont) =>
            me.remove(eff).fold({ otherEffect =>
              val newCont = Eff.compose(cont) andThen { loop(jq, _) }
              Impure[R, L0, X, Option[(A, Eff[R, L0, A])]](
                eff, Queue.one[Arr_[R, L0]#O, X, Option[(A, Eff[R, L0, A])]](newCont))
            },
              _.fold({
                jq.toNel.fold[Eff[R, L0, Option[(A, Eff[R, L0, A])]]](Pure[R, L0, Option[(A, Eff[R, L0, A])]](None))({
                  jqn => loop(jqn.tail.toList, jqn.head)
                })
              }, { le =>
                val booleanCont = Eff.compose(le.subst[({ type K[Y] = Arrs[R, L0, Y, A] })#K](cont))
                loop(booleanCont(false) :: jq, booleanCont(true))
              }))
        }
      })

  def msplit[R <: Coproduct, L0 <: Layers[R], A](eff: Eff[R, L0, A])(
    implicit su: Subset[R, NDet_ :+: CNil] {
      type LS = L0
      type LT = Layers.One[NDet_]
    }, me: Member[R, NDet_] { type L = L0 }): Eff[R, L0, Option[(A, Eff[R, L0, A])]] =
    loop(Nil, eff)

  def ifte[R <: Coproduct, L0 <: Layers[R], A, B](t: Eff[R, L0, A], th: Arr[R, L0, A, B], el: Eff[R, L0, B])(
    implicit su: Subset[R, NDet_ :+: CNil] {
      type LS = L0
      type LT = Layers.One[NDet_]
    }, me: Member[R, NDet_] { type L = L0 }): Eff[R, L0, B] =
    msplit(t).flatMap(_.fold(el)(u => th(u._1)))
}