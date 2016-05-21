package paperdoll.scalaz

import shapeless.{ :+:, Coproduct, CNil }
import paperdoll.core.layer.Layers
import paperdoll.core.effect.{ Effects, Arr, GenericBind, GenericHandler }
import paperdoll.core.effect.Effects.sendU
import scalaz.Id.Id
import scalaz.Reader
import scala.Predef.identity
import scalaz.MonadListen
import paperdoll.core.layer.Member
import paperdoll.core.layer.Subset
import paperdoll.core.effect.Handler
import paperdoll.core.layer.Layer
import paperdoll.core.effect.GenericTranslator
import paperdoll.core.effect.Bind
import scalaz.syntax.monad._

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

  def translateReader[F[_], I](implicit ml: MonadListen[F, I]): GenericTranslator[Reader_[I]] {
    type OR = Layer.Aux[F] :+: CNil
    type OL = Layers.One[Layer.Aux[F]]
  } =
    new GenericTranslator[Reader_[I]] {
      override type OR = Layer.Aux[F] :+: CNil
      override type OL = Layers.One[Layer.Aux[F]]
      override def handler[R <: Coproduct, L1 <: Layers[R], RR <: Coproduct, RL <: Layers[RR]](
        implicit me1: Member[R, Reader_[I]] { type L = L1; type RestR = RR; type RestL = RL },
        su: Subset[RR, OR] { type LT = OL; type LS = RL }): Handler[R, L1, Reader_[I]] { type RestR = RR; type RestL = RL; type O[X] = X } =
        new Bind[R, L1, Reader_[I]] {
          override type RestR = RR
          override type RestL = RL
          override type O[X] = X
          override def me = me1
          override def pure[A](a: A) = a
          override def bind[V, A](eff: Reader[I, V], cont: V ⇒ Effects[RR, RL, A]): Effects[RR, RL, A] = {
            sendU(ml.listen(ml.point({}))).extend[RR]().flatMap {
              x ⇒
                val (_, i) = x
                cont(eff.run(i))
            }
          }
        }
    }
}