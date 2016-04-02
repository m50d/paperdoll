package paperdoll.core.layer

import org.junit.Test
import shapeless.{ CNil, :+: }
import paperdoll.reader.Reader_
import paperdoll.scalaz.Disjunction_

class SubsetTest {
  @Test def basicFunctionality(): Unit = {
    val _1 = Subset[Reader_[String] :+: Reader_[Int] :+: CNil, CNil]
    val _2 = Subset[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[Int] :+: CNil]
    val _3 = Subset[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[String] :+: CNil]
    val _4 = Subset[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[Int] :+: Reader_[String] :+: CNil]
  }
  
  @Test def disjunction(): Unit = {
    val _1 = Subset[Reader_[String] :+: Disjunction_[Int] :+: CNil, Disjunction_[Int] :+: CNil]
  }
  
  def parameterized[F[_], A](): Unit = {
    val _1 = Subset[Layer.Aux[F] :+: Disjunction_[A] :+: CNil, Disjunction_[A] :+: CNil]
  }
}