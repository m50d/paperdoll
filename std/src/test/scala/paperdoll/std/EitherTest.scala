package paperdoll.std

import org.junit.Test
import scalaz.std.either._
import EitherLayer._
import paperdoll.core.effect.Effects._
import org.fest.assertions.Assertions.assertThat

class EitherTest {
	@Test def basicFunctionality(): Unit = {
	  val either: Either[Int, String] = Left(4)
	  val eff = send[Either_[Int], String](either)
	  val _ = assertThat(handleEither[Int](eff).run).isEqualTo(Left(4))
	}
}