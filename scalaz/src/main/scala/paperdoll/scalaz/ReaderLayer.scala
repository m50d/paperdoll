package paperdoll.scalaz

import shapeless.{ :+:, Coproduct, CNil }
import paperdoll.core.layer.Layers
import paperdoll.core.effect.{ Effects, Arr, GenericBind, GenericHandler }
import paperdoll.core.effect.Effects.sendU
import scalaz.Id.Id
import scalaz.Reader
import scala.Predef.identity
import paperdoll.core.layer.Layer
import paperdoll.core.effect.GenericTranslator
import paperdoll.core.effect.GenericSingleTranslator
import scalaz.MonadReader

object ReaderLayer {
  def sendReader[I, A](reader: Reader[I, A]): Effects.One[Reader_[I], A] =
    sendU(reader)
  /** Effect that reads an input I and returns it.
   */
  def sendAsk[I]: Effects.One[Reader_[I], I] =
    sendReader(Reader(identity[I]))

  /** Run the reader effect in the stack R by passing the input i
   *  (i.e. giving the value i to any reads in the "lazy effectful value" e),
   *  removing Reader_[I] from the stack of effects in the result.
   */
  def handleReader[I](i: I): GenericHandler.Aux[Reader_[I], Id] = new GenericBind[Reader_[I]] {
    override type O[X] = X
    override def pure[A](a: A) = a
    override def bind[V, RR <: Coproduct, RL <: Layers[RR], A](reader: Reader[I, V], arr: Arr[RR, RL, V, A]) =
      arr(reader(i))
  }

  def translateReader[F[_], I](implicit mr: MonadReader[F, I]): GenericTranslator[Reader_[I]] {
    type OR = Layer.Aux[F] :+: CNil
    type OL = Layers.One[Layer.Aux[F]]
  } = new GenericSingleTranslator[Reader_[I]] {
    override type O = Layer.Aux[F]
    override def handle[V](eff: Reader[I, V]) =
      sendU(mr.asks(eff.run))
  }
}