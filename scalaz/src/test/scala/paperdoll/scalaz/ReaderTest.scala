package paperdoll.scalaz

import shapeless.{ CNil, :+: }
import org.junit.Test
import scalaz.syntax.monad._
import ReaderLayer._
import org.fest.assertions.Assertions.assertThat
import scalaz.ReaderWriterState
import paperdoll.core.layer.Layer
import scalaz.std.anyVal._
import scalaz.Id.Id
import paperdoll.core.effect.Effects
import scalaz.Monad

class ReaderTest {
  val reader =
    for {
      fst ← sendAsk[Int]
      snd ← sendAsk[Int]
    } yield fst + snd

  @Test def basicFunctionality(): Unit = {
    val _ = assertThat(handleReader(4)(reader).run).isEqualTo(8)
  }

  @Test def differingOrders(): Unit = {
    val eff = for {
      count ← sendAsk[Int].extend[Reader_[String] :+: Reader_[Int] :+: CNil]()
      label ← sendAsk[String].extend[Reader_[String] :+: Reader_[Int] :+: CNil]()
    } yield f"There are $count%d $label%s"

    val _1 = assertThat(handleReader(4)(handleReader("lights")(eff)).run).isEqualTo("There are 4 lights")
    val _2 = assertThat(handleReader("lights")(handleReader(4)(eff)).run).isEqualTo("There are 4 lights")
  }

  @Test def translate(): Unit = {
    type MyScalaZStack[A] = ReaderWriterState[Int, Unit, Unit, A]
    type MyPaperdollStack = Reader_[Int] :+: Layer.Aux[MyScalaZStack] :+: CNil
    val translatedStack = translateReader[MyScalaZStack, Int].apply(reader.extend[MyPaperdollStack]())
    //TODO: Extra monad argument should be unnecessary once SI-7212 (?) is fixed
    val scalazStack = Effects.unsafeRun(translatedStack)(Monad[MyScalaZStack])
    val _ = assertThat(scalazStack.run(4, {})).isEqualTo(({}, 8, {}))
  }
}