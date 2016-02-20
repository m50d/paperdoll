package com.github.m50d.paperdoll

import org.junit.Test
import org.fest.assertions.Assertions.assertThat
import org.junit.Assert.fail
import com.github.m50d.paperdoll.queue.Queue
import com.github.m50d.paperdoll.queue.Q0
import com.github.m50d.paperdoll.queue._
import com.github.m50d.paperdoll.queue.TAEmptyL
import com.github.m50d.paperdoll.queue.:<

class QueueTest {
  @Test def basicFunctionality(): Unit = {
    val queue: Queue[Function1, Int, Int] = Q0[Function1, Int]() |> (_.toString) |> (_.length)
    queue.tviewl match {
      case (ca: :<[Queue, Function1, Int, String, Int] @unchecked) =>
        assertThat(ca.e(4)).isEqualTo("4")
        ca.s.tviewl match {
          case (ca: :<[Queue, Function1, String, Int, Int] @unchecked) =>
            assertThat(ca.e("hello")).isEqualTo(5)
            ca.s.tviewl match {
              case TAEmptyL() =>
              case _ => fail()
            }
          case _ => fail()
        }
      case _ => fail()
    }
  }
}