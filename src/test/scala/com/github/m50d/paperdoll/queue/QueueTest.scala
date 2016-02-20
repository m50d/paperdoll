package com.github.m50d.paperdoll.queue

import org.junit.Test
import org.fest.assertions.Assertions.assertThat
import org.junit.Assert.fail
import scalaz.Forall

class QueueTest {
  /**
   * This test uses asInstanceOf to access the internals of Queue -
   * user code is not supposed to do this
   */
  @Test def basicFunctionality(): Unit = {
    val queue: Queue[Function1, Int, Int] = Queue.empty[Function1, Int] :+ (_.toString) :+ (_.length)
    queue.destructureHead.fold({
      _ => fail()
    },
      new Forall[({ type L[X] = (Function1[Int, X], Queue[Function1, X, Int]) => Unit })#L] {
        override def apply[X] = {
          (head, tail) =>
            assertThat(head.asInstanceOf[Int => String](4)).isEqualTo("4")
            tail.asInstanceOf[Queue[Function1, String, Int]].destructureHead.fold({
              _ => fail()
            },
              new Forall[({ type L[X] = (Function1[String, X], Queue[Function1, X, Int]) => Unit })#L] {
                override def apply[X] = {
                  (head, tail) =>
                    assertThat(head.asInstanceOf[String => Int]("hello")).isEqualTo(5)
                    tail.asInstanceOf[Queue[Function1, Int, Int]].destructureHead.fold({
                      _ =>
                    }, new Forall[({ type L[X] = (Function1[Int, X], Queue[Function1, X, Int]) => Unit })#L] {
                      override def apply[X] = {
                        (head, tail) =>
                          fail()
                      }
                    })

                }
              })
        }
      })
  }
}