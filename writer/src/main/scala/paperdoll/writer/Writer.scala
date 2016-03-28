package paperdoll.writer

import shapeless.Coproduct
import scalaz.{Leibniz, Monoid}
import scalaz.Leibniz.===
import scalaz.syntax.functor._
import scalaz.syntax.monoid._
import paperdoll.core.effect.{ Eff, Arr, Bind, Handler }
import paperdoll.core.layer.Layers

/**
 * Type representing an effectful value of type X
 * that writes to an output of type O.
 * Implementation is encapsulated.
 */
sealed trait Writer[O, X] {
  def fold[A](put: (O, X === Unit) ⇒ A): A
}

object Writer {
  private[this] def put[O](o: O) = new Writer[O, Unit] {
    override def fold[A](put: (O, Unit === Unit) ⇒ A) = put(o, Leibniz.refl)
  }

  /**
   * Effect that writes the value O
   */
  def tell[O](o: O): Eff.One[Writer_[O], Unit] =
    Eff.send[Writer_[O], Unit](put(o))

  /**
   * Run the writer effect, producing a vector of all the written values
   */
  def runWriterVector[O0]: Handler[Writer_[O0]] {
    type O[X] = (X, Vector[O0])
  } = Eff.handle(new Bind[Writer_[O0]] {
    override type O[X] = (X, Vector[O0])
    override def pure[A](a: A) = (a, Vector())
    override def apply[V, RR <: Coproduct, RL <: Layers[RR], A](writer: Writer[O0, V], arr: Arr[RR, RL, V, O[A]]) =
      writer.fold {
        (o, le) ⇒
          le.subst[({ type L[X] = Arr[RR, RL, X, O[A]] })#L](arr)({}) map { case (a, l) ⇒ (a, o +: l) }
      }
  })
  
  /**
   * Run the writer effect, merging all the written values.
   * Notice how we can have multiple interpreters for the same effect,
   * as we've decoupled the declaration of an effect from its implementation.
   */
  def runWriterMonoid[O0: Monoid]: Handler[Writer_[O0]] {
    type O[X] = (X, O0)
  } = Eff.handle(new Bind[Writer_[O0]] {
    override type O[X] = (X, O0)
    override def pure[A](a: A) = (a, Monoid[O0].zero)
    override def apply[V, RR <: Coproduct, RL <: Layers[RR], A](writer: Writer[O0, V], arr: Arr[RR, RL, V, O[A]]) =
      writer.fold {
        (o, le) ⇒
          le.subst[({ type L[X] = Arr[RR, RL, X, O[A]] })#L](arr)({}) map { case (a, l) ⇒ (a, o |+| l) }
      }
  })
}