//package com.github.m50d.paperdoll.reader
//
//import shapeless.{ Coproduct, CNil, :+: }
//import shapeless.ops.coproduct.Inject
//import scalaz.syntax.monad._
//import scalaz.{ Forall, Leibniz }
//import scalaz.Leibniz.===
//import com.github.m50d.paperdoll.layer.Layers
//import com.github.m50d.paperdoll.effect.{ Eff, Eff_, Arr }
//import com.github.m50d.paperdoll.layer.Member
//import com.github.m50d.paperdoll.effect.Bind
//import com.github.m50d.paperdoll.effect.Handler
//
///**
// * The type representing an effectful value of type X
// * that reads from input of type I
// * Implementation is encapsulated (hopefully).
// */
//sealed trait Reader[I, X] {
//  def fold[A](get: (I === X) ⇒ A): A
//}
//private[reader] final case class Get[I, X](val witness: Leibniz.===[I, X]) extends Reader[I, X] {
//  override def fold[A](get: (I === X) ⇒ A) = get(witness)
//}
//
//object Reader {
//  /**
//   * Effect that reads an input I and returns it.
//   */
//  def ask[I]: Eff[Reader_[I] :+: CNil, Layers[Reader_[I] :+: CNil] { type O[X] = Reader[I, X] :+: CNil }, I] =
//    Eff.send[Reader_[I], I](Get[I, I](Leibniz.refl))
//
//  /**
//   * Run the reader effect in the stack R by passing the input i
//   * (i.e. giving the value i to any reads in the "lazy effectful value" e),
//   * removing Reader_[I] from the stack of effects in the result.
//   */
//  def runReader[I](i: I): Handler[Reader_[I]] = Eff.handle(new Bind[Reader_[I]] {
//    override def apply[V, RR <: Coproduct, RL <: Layers[RR], A](reader: Reader[I, V], arr: Arr[RR, RL, V, A]) =
//      reader.fold(witness ⇒ arr(witness(i)))
//  })
//}