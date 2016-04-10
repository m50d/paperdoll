package paperdoll.scalaz

import scalaz.ReaderT
import scalaz.Reader
import paperdoll.core.effect.Effects.sendTU
import scalaz.Functor
import paperdoll.core.effect.Effects
import paperdoll.core.layer.Layer

object ReaderTLayer {
   def sendReaderT[F[_]: Functor, I, A](readerT: ReaderT[F, I, A]): Effects.Two[Reader_[I], Layer.Aux[F], A] = 
     sendTU[Reader[I, F[A]], F[A]](Reader(readerT.run))
}