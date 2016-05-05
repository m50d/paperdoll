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
import paperdoll.core.layer.Layer
import paperdoll.core.layer.Layers
import paperdoll.core.layer.Member
import scalaz.Leibniz
import paperdoll.core.layer.Subset
import paperdoll.core.effect.Effects.unsafeRun

class WriterTest {
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
    val translatedEffect = translator.apply[Writer_[Int] :+: Layer.Aux[Tgt] :+: CNil, Layers.Two[Writer_[Int], Layer.Aux[Tgt]],
      String, Layers.Two[Writer_[Int], Layer.Aux[Tgt]], Layer.Aux[Tgt] :+: CNil, Layers.One[Layer.Aux[Tgt]]
    ](extendedEffect)(Member.nil[Writer_[Int], Layer.Aux[Tgt] :+: CNil], Leibniz.refl[Layers.Two[Writer_[Int], Layer.Aux[Tgt]]],
        Subset[Layer.Aux[Tgt] :+: CNil, Layer.Aux[Tgt] :+: CNil])
    val overall = unsafeRun(translatedEffect)
    val _1 = assertThat(overall.run).isEqualTo(Some((6, "WriterTest")))
  }
}