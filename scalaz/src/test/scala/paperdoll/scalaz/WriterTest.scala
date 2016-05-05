package paperdoll.scalaz

import org.junit.Test
import scalaz.syntax.monad._
import scalaz.std.anyVal._
import WriterLayer._
import org.fest.assertions.Assertions.assertThat
import scala.collection.immutable.BitSet
import scalaz.WriterT
import scalaz.std.option._
import shapeless.{:+:, CNil}
import paperdoll.core.effect.Effects.unsafeRun
import paperdoll.core.layer.Layer

class WritterTest {
  val effect = for {
      _ <- sendTell(1)
      _ <- sendTell(3)
      _ <- sendTell(2)
    } yield "WriterTest"
  
  @Test def basicFunctionality(): Unit = {
    val _1 = assertThat(handleWriterCollection[Int, Vector[Int]].apply(effect).run).isEqualTo((Vector(1, 3, 2), "WriterTest"))
    val _2 = assertThat(handleWriterMonoid[Int].apply(effect).run).isEqualTo((6, "WriterTest"))
    val _3 = assertThat(handleWriterCollection[Int, BitSet].apply(effect).run).isEqualTo((BitSet(1, 2, 3), "WriterTest"))
  }
  
  @Test def translate(): Unit = {
    type Tgt[A] = WriterT[Option, Int, A]
    val translator = translateWriter[Tgt, Int]
    val extendedEffect = effect.extend[Writer_[Int] :+: Layer.Aux[Tgt] :+: CNil]()
    val translatedEffect = translator(extendedEffect)
    val overall = unsafeRun(translatedEffect)
    val _1 = assertThat(overall.run).isEqualTo(Some((6, "WriterTest")))
  }
}