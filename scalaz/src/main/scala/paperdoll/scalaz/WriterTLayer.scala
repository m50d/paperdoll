package paperdoll.scalaz

import scalaz.Functor
import scalaz.WriterT
import paperdoll.core.effect.Effects
import scalaz.syntax.functor._
import scalaz.Writer

object WriterTLayer {
  def sendWriterT[F[_]: Functor, A, B](wt: WriterT[F, A, B]) =
    Effects.sendTU(wt.run.map({wa => Writer(wa._1, wa._2)}))
}