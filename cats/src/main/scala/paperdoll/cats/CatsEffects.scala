package paperdoll.cats

import cats.Unapply
import paperdoll.core.effect.Effects
import paperdoll.core.effect.Effects.send
import cats.Functor
import paperdoll.core.layer.Layer
import shapeless.{:+:, CNil}
import scalaz.syntax.monad._

object CatsEffects {
  def sendUC[FV](value: FV)(implicit u: Unapply[Functor, FV]): Effects.One[Layer.Aux[u.M], u.A] =
    send[Layer.Aux[u.M], u.A](u.subst(value))

  def sendTUC[FGA, GA](value: FGA)(implicit u1: Unapply[Functor, FGA] {
    type A = GA
  }, u2: Unapply[Functor, GA]): Effects.Two[Layer.Aux[u1.M], Layer.Aux[u2.M], u2.A] = {
    // Inlining this causes compilation to fail, I don't understand why
    def sendGA(ga: GA) = sendUC(ga).extend[Layer.Aux[u1.M] :+: Layer.Aux[u2.M] :+: CNil]()
    sendUC(value).extend[Layer.Aux[u1.M] :+: Layer.Aux[u2.M] :+: CNil]().flatMap(sendGA(_))
  }
}