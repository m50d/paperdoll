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

  //TODO remove duplication between this and the other case
  implicit def monadPlus[R <: Coproduct, L <: Layers[R], LT0 <: Layers[NDet_ :+: CNil]](implicit su: Subset[R, NDet_ :+: CNil] {
    type LS = L
    type LT = LT0
  }, le: Leibniz[Nothing, Layers[NDet_ :+: CNil], LT0, Layers.One[NDet_]#N]): MonadPlus[Eff_[R, L]#O] =
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
  
//  private[this] def loop[R <: Coproduct, L <: Layers[R], A, LT0 <: Layers[NDet_ :+: CNil]](
//      jq: List[Eff[R, L, A]], j: Eff[R, L, A])(implicit su: Subset[R, NDet_ :+: CNil] {
//    type LS = L
//    type LT = LT0
//  }, le: Leibniz[Nothing, Layers[NDet_ :+: CNil], LT0, Layers.One[NDet_]#N]): Eff[R, L, Option[(A, Eff[R, L, A])]] =
//    j.fold(a => Some((a, jq.msumlU)), ???)
  
  def msplit[R <: Coproduct, L <: Layers[R], A, LT0 <: Layers[R]](eff: Eff[R, L, A]): Eff[R, L, Option[(A, Eff[R, L, A])]] =
    ???
}