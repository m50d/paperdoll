package paperdoll.scalaz

import org.fest.assertions.Assertions.assertThat
import org.junit.Test
import EitherTLayer._
import paperdoll.core.effect.Effects
import paperdoll.core.effect.Effects.unsafeRun
import paperdoll.std.OptionLayer._
import scalaz.std.option._
import paperdoll.std.Option_
import shapeless.CNil
import paperdoll.core.layer.Layers
import shapeless.:+:
import scalaz.Disjunction
import scalaz.EitherT
import scalaz.\/-

object EitherTLayerTest {
  @Test def basicFunctionality(): Unit = {
    val eff: Effects[Option_ :+: Disjunction_[Int] :+: CNil, Layers[Option_ :+: Disjunction_[Int] :+: CNil] {
      type O[X] = Option[X] :+: Disjunction[Int, X] :+: CNil
    }, String] = sendEitherT[Option, Int, String](EitherT.right[Option, Int, String](Some("test")))
    val partial = handleOption(eff)
    val pure = unsafeRun(partial)
    val _ = assertThat(pure).isEqualTo(\/-(Some("test")))
  }
}