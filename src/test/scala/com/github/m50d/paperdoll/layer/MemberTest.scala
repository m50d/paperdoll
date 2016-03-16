package com.github.m50d.paperdoll.layer

import org.junit.Test
import shapeless.{ CNil, :+: }
import paperdoll.reader.Reader_

class MemberTest {

  @Test def basicFunctionality(): Unit = {
    val _1 = Member[Reader_[Int] :+: CNil, Reader_[Int]]
    val _2 = Member[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[String]]
    val _3 = Member[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[Int]]
  }
}