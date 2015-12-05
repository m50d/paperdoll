package com.github.m50d.paperdoll

import shapeless.Coproduct
import shapeless.CNil
import shapeless.:+:
import shapeless.ops.coproduct.Inject
import aliases._
import scalaz.syntax.monad._
import scala.annotation.tailrec

trait Reader[I, X]

case class Get[I]() extends Reader[I, I]

object example {
  final type Reader_[I] = Layer {
    type F[X] = Reader[I, X]
  }
  
  def ask[I, R <: Coproduct, F[_] <: Coproduct](implicit l: Layers.Aux[R, F], inj: Inject[F[I], Reader_[I]#F[I]]): Eff[R, I] =
    Eff.send[Reader_[I]#F, R, F, I](Get[I]())
    
  def askReaderOnly[I]: Eff[Reader_[I] :+: CNil, I] =
    ask[I, Reader_[I] :+: CNil, ({type L[X] = Reader[I, X] :+: CNil})#L]
  
  def addGet[R <: Coproduct, F[_] <: Coproduct](x: Int)(
      implicit l: Layers.Aux[R, F], inj: Inject[F[Int], Reader_[Int]#F[Int]]): Eff[R, Int] =
    Eff.monadEff.bind(ask[Int, R, F](l, inj)){i => (i+x).point[Eff_[R]#O]}
  
  def runReader[I, R <: Coproduct, A](i: I, e: Eff[Reader_[I] :+: R, A]): Eff[R, A] = {
//   def loop(m: Eff[Reader_[I] :+: R, A]): Eff[R, A] = m match {
//      case Pure(x) => x.point[Eff_[R]#O]
//      case i: Impure[R, A] =>
//        i.eff.removeElemC[Reader_[I]#F[i.X]] match {
//          case _ => ???
//        }
//        ???
//    }
    ???
  }
    
}