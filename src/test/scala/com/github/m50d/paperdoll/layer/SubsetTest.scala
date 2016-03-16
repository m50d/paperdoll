package com.github.m50d.paperdoll.layer

import org.junit.Test
import shapeless.{ CNil, :+: }
import paperdoll.reader.Reader_

class SubsetTest {
  @Test def basicFunctionality(): Unit = {
    val _1 = Subset[Reader_[String] :+: Reader_[Int] :+: CNil, CNil]
    val _2 = Subset[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[Int] :+: CNil]
    val _3 = Subset[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[String] :+: CNil]
    val _4 = Subset[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[Int] :+: Reader_[String] :+: CNil]
  }
}