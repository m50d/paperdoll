package paperdoll.core.layer

import org.junit.Test
import shapeless.{:+:, CNil}
import paperdoll.scalaz.Disjunction_

class LayersTest {
	@Test def disjunction(): Unit = {
	  Layers[Disjunction_[String] :+: CNil]
	}
	def parameterized[A](): Unit = {
	  Layers[Disjunction_[A] :+: CNil]
	}
}