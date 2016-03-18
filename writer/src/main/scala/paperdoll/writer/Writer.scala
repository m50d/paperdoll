package paperdoll.writer

import shapeless.{:+:, CNil}
import paperdoll.core.effect.Eff
import paperdoll.core.layer.Layers

/**
 * Type representing an effectful value of type X
 * that writes to an output of type O.
 * Implementation is encapsulated.
 */
sealed trait Writer[O, X] {
  def fold[A](put: O => A): A
}

object Writer {
  private[this] def put[O](o: O) = new Writer[O, Unit] {
    override def fold[A](put: O => A) = put(o)
  }
  
  def tell[O0](o: O0): Eff[Writer_[O0] :+: CNil, Layers[Writer_[O0] :+: CNil] {type O[X] = Writer[O0, X] :+: CNil}, Unit] =
    Eff.send[Writer_[O0], Unit](put(o))
}