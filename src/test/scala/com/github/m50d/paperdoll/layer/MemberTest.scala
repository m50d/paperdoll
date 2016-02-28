package com.github.m50d.paperdoll.layer

import org.junit.Test
import shapeless.{CNil, :+:}
import com.github.m50d.paperdoll.reader.Reader_

class MemberTest {

  @Test def basicFunctionality(): Unit = {
    val l = Layers[Reader_[Int] :+: CNil]
    Member[Reader_[Int] :+: CNil, l.type, Reader_[Int]]
  }
}