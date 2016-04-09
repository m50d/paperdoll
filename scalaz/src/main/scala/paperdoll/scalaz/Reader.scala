package paperdoll.scalaz

import shapeless.Coproduct
import scalaz.Leibniz
import paperdoll.core.layer.Layers
import paperdoll.core.effect.{ Effects, Arr, Bind, Handler }
import scalaz.Id.Id
import scalaz.Leibniz.===

/**
 * Type representing an effectful value of type X
 * that reads from input of type I.
 * Implementation is encapsulated.
 */
sealed trait Reader[I, X] {
  def fold[A](get: (I === X) ⇒ A): A
}
object Reader {
  private[this] def get[I] = new Reader[I, I] {
    override def fold[A](get: (I === I) ⇒ A) = get(Leibniz.refl)
  }
  
  /**
   * Effect that reads an input I and returns it.
   */
  def ask[I]: Effects.One[Reader_[I], I] =
    Effects.send[Reader_[I], I](get)

  /**
   * Run the reader effect in the stack R by passing the input i
   * (i.e. giving the value i to any reads in the "lazy effectful value" e),
   * removing Reader_[I] from the stack of effects in the result.
   */
  def handleReader[I](i: I): Handler.Aux[Reader_[I], Id] = Effects.handle(new Bind[Reader_[I]] {
    override type O[X] = X
    override def pure[A](a: A) = a
    override def apply[V, RR <: Coproduct, RL <: Layers[RR], A](reader: Reader[I, V], arr: Arr[RR, RL, V, A]) =
      reader.fold(witness ⇒ arr(witness(i)))
  })
}