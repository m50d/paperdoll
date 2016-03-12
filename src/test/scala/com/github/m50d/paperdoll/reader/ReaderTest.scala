//package com.github.m50d.paperdoll.reader
//
//import org.junit.Test
//import scalaz.syntax.monad._
//import Reader._
//import com.github.m50d.paperdoll.effect.Eff
//import shapeless.{ CNil, :+: }
//import org.fest.assertions.Assertions.assertThat
//import com.github.m50d.paperdoll.layer.Layers
//import com.github.m50d.paperdoll.layer.Member
//import scalaz.Leibniz
//import com.github.m50d.paperdoll.layer.Layer
//
//class ReaderTest {
//  @Test def basicFunctionality(): Unit = {
//    val reader =
//      for {
//        fst <- ask[Int]
//        snd <- ask[Int]
//      } yield fst + snd
//
//    val pure = runReader(4)(reader)
//    val result = Eff.run(pure)
//    assertThat(result).isEqualTo(8)
//  }
//  
////  @Test def differingOrders(): Unit = {
////    for {
////      count <- ask[Int].extend[Reader_[String] :+: Reader_[Int] :+: CNil]()
////      label <- ask[String].extend[Reader_[String] :+: Reader_[Int] :+: CNil]()
////    } yield f"There are $count%d $label%s"
////  }
//}