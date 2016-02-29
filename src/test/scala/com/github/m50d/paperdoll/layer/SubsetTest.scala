package com.github.m50d.paperdoll.layer

import org.junit.Test
import shapeless.{CNil, :+:}
import com.github.m50d.paperdoll.reader.Reader_

class SubsetTest {
  @Test def basicFunctionality(): Unit = {
    val lsi = Layers[Reader_[String] :+: Reader_[Int] :+: CNil]
    val li = Layers[Reader_[Int] :+: CNil]
    
    Subset[Reader_[String] :+: Reader_[Int] :+: CNil, CNil]
//    Subset[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[Int] :+: CNil](
//      Subset.cons[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[Int], CNil](
//        SubsetHelper.help    
//      )
//    )
  }

}