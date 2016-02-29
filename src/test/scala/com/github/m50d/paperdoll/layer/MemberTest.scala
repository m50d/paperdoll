package com.github.m50d.paperdoll.layer

import org.junit.Test
import shapeless.{ CNil, :+: }
import com.github.m50d.paperdoll.reader.Reader_

class MemberTest {

  @Test def basicFunctionality(): Unit = {
    Member[Reader_[Int] :+: CNil, Reader_[Int]]
    Member[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[String]]
    Member[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[Int]]
    implicitly[Member[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[Int]]]
  }
}