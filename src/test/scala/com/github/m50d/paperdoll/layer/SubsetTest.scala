package com.github.m50d.paperdoll.layer

import org.junit.Test
import shapeless.{ CNil, :+: }
import com.github.m50d.paperdoll.reader.Reader_

class SubsetTest {
  @Test def basicFunctionality(): Unit = {
    Subset[Reader_[String] :+: Reader_[Int] :+: CNil, CNil]
    Subset[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[Int] :+: CNil]
    Subset[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[String] :+: CNil]
    Subset[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[Int] :+: Reader_[String] :+: CNil]
  }
}