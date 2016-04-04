package paperdoll.std

import org.junit.Test
import scalaz.std.either._
import paperdoll.core.effect.Effects
import org.fest.assertions.Assertions.assertThat

class EitherLayerTest {
	@Test def basicFunctionality(): Unit = {
	  val either: Either[Int, String] = Left(4)
	  val eff = Effects.sendU(either)
	  val _ = assertThat(EitherLayer.runEither[Int](eff).run).isEqualTo(Left(4))
	}
}