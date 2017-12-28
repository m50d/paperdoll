package paperdoll.scalaz

import shapeless.{:+:, CNil}
import scalaz.syntax.monad._
import scalaz.ReaderT
import scalaz.Reader
import paperdoll.core.effect.Effects.send
import scalaz.Functor
import paperdoll.core.effect.Effects
import paperdoll.core.layer.Layer

object ReaderTLayer {
   def sendReaderT[F[_]: Functor, I, A](readerT: ReaderT[F, I, A]): Effects.Two[Reader_[I], Layer.Aux[F], A] = {
     def sendF(fa: F[A]): Effects.Two[Reader_[I], Layer.Aux[F], A] =
       send[Layer.Aux[F], A](fa).extend[Reader_[I] :+: Layer.Aux[F] :+: CNil]
     send[Reader_[I], F[A]](readerT.run).extend[Reader_[I] :+: Layer.Aux[F] :+: CNil].flatMap(sendF)
   }
}