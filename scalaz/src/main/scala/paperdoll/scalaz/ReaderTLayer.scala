package paperdoll.scalaz

import scalaz.ReaderT
import scalaz.Reader
import paperdoll.core.effect.Effects.sendTU
import scalaz.Functor

object ReaderTLayer {
   def sendReaderT[F[_]: Functor, I, A](readerT: ReaderT[F, I, A]) = 
     sendTU(Reader(readerT.run))
}