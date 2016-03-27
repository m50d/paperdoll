package paperdoll.arm

import resource.Resource
import paperdoll.core.effect.Eff
import resource.ManagedResource
import resource.managed
import scala.reflect.Manifest
import scalaz.Leibniz
import paperdoll.core.layer.Member
import paperdoll.core.layer.Layers
import shapeless.Coproduct
import scalaz.Leibniz.===
import shapeless.UnaryTCConstraint
import paperdoll.core.effect.Pure

sealed trait Region[S, R, A] {
  def fold[B](resource: (A === R, ManagedResource[R]) => B): B
}

object Region {
  def newSHandle[R: Resource: Manifest](r: => R): Eff.One[Region_[S, R], R]#O forSome { type S } =
    Eff.send[Region_[Any, R], R](new Region[Any, R, R] {
      override def fold[B](resource: (R === R, ManagedResource[R]) => B) =
        resource(Leibniz.refl, managed(r))
    })
    
//  private[this] def open[S, RE, R <: Coproduct, L1 <: Layers[R], A](resource: eff: Eff[R, L1, A])(implicit me: Member[R, L] { type L = L1 }):
//    Eff[me.RestR, me.RestL, O[A]]
//  
//  eff.fold({ a ⇒ Pure[me.RestR, me.RestL, A](a) }, new Forall[({ type K[X] = (me.L#O[X], Arrs[R, me.L, X, A]) ⇒ Eff[me.RestR, me.RestL, O[A]] })#K] {
//      override def apply[X] = { (eff, cont) ⇒
//        //New continuation is: recursively run this handler on the result of the old continuation 
//        val newCont = compose(cont) andThen { run(_) }
//        me.remove(eff).fold(
//          otherEffect ⇒ Impure[me.RestR, me.RestL, X, A](otherEffect, Queue.one[Arr_[me.RestR, me.RestL]#O, X, O[A]](newCont)),
//          thisEffect ⇒ bind[X, me.RestR, me.RestL, A](thisEffect, newCont))
//      }
//    })
//
//  def newRgn[S, RE, R <: Coproduct, L1 <: Layers[R], A, L2 <: Layers[R]](eff: Eff[R, L1, A])(
//    implicit me: Member[R, Region_[S, RE]] { type L = L2 },
//    le: Leibniz[Nothing, Layers[R], L1, L2] /* TODO ,
//    safe: UnaryTCConstraint[R, SafeForRegion]*/ ): Eff[me.RestR, me.RestL, A] =
//    ???
}