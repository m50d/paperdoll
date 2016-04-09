package paperdoll.scalaz

import shapeless.{CNil, :+:}
import org.junit.Test
import scalaz.syntax.monad._
import Reader._
import org.fest.assertions.Assertions.assertThat

class ReaderTest {
  @Test def basicFunctionality(): Unit = {
    val reader =
      for {
        fst ← ask[Int]
        snd ← ask[Int]
      } yield fst + snd

    val _ = assertThat(handleReader(4)(reader).run).isEqualTo(8)
  }

  @Test def differingOrders(): Unit = {
    val eff = for {
      count ← ask[Int].extend[Reader_[String] :+: Reader_[Int] :+: CNil]()
      label ← ask[String].extend[Reader_[String] :+: Reader_[Int] :+: CNil]()
    } yield f"There are $count%d $label%s"
    
    val _1 = assertThat(handleReader(4)(handleReader("lights")(eff)).run).isEqualTo("There are 4 lights")
    val _2 = assertThat(handleReader("lights")(handleReader(4)(eff)).run).isEqualTo("There are 4 lights")
  }
}