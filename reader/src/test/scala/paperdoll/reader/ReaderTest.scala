package paperdoll.reader

import shapeless.{CNil, :+:}
import org.junit.Test
import scalaz.syntax.monad._
import Reader._
import paperdoll.core.effect.Eff
import org.fest.assertions.Assertions.assertThat

class ReaderTest {
  @Test def basicFunctionality(): Unit = {
    val reader =
      for {
        fst ← ask[Int]
        snd ← ask[Int]
      } yield fst + snd

    val pure = runReader(4)(reader)
    val result = Eff.run(pure)
    val _ = assertThat(result).isEqualTo(8)
  }

  @Test def differingOrders(): Unit = {
    val eff = for {
      count ← ask[Int].extend[Reader_[String] :+: Reader_[Int] :+: CNil]()
      label ← ask[String].extend[Reader_[String] :+: Reader_[Int] :+: CNil]()
    } yield f"There are $count%d $label%s"
    
    runReader(4)(eff)
    val pure1 = runReader(4)(runReader("lights")(eff))
    val _1 = assertThat(Eff.run(pure1)).isEqualTo("There are 4 lights")
    val pure2 = runReader("lights")(runReader(4)(eff))
    val _2 = assertThat(Eff.run(pure2)).isEqualTo("There are 4 lights")
  }
}