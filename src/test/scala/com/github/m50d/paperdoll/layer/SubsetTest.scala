package com.github.m50d.paperdoll.layer

import org.junit.Test
import shapeless.{ CNil, :+: }
import com.github.m50d.paperdoll.reader.Reader_

class SubsetTest {
  @Test def basicFunctionality(): Unit = {
    val lsi = Layers[Reader_[String] :+: Reader_[Int] :+: CNil]
    val li = Layers[Reader_[Int] :+: CNil]

    Subset[Reader_[String] :+: Reader_[Int] :+: CNil, Layers.Aux[Reader_[String] :+: Reader_[Int] :+: CNil, lsi.O], CNil, Layers[CNil] {
      type O[X] = CNil
    }]
//    Element[Reader_[String] :+: Reader_[Int] :+: CNil, Layers.Aux[Reader_[String] :+: Reader_[Int] :+: CNil, lsi.O], Reader_[Int]]
//    Subset[Reader_[String] :+: Reader_[Int] :+: CNil, Layers.Aux[Reader_[String] :+: Reader_[Int] :+: CNil, li.O], Reader_[Int] :+: CNil, Layers.Aux[Reader_[Int] :+: CNil, li.O]](
//      Subset.consSubset[Reader_[String] :+: Reader_[Int] :+: CNil, Layers.Aux[Reader_[String] :+: Reader_[Int] :+: CNil, li.O], Reader_[Int], CNil, Layers[CNil] {
//        type O[X] = CNil
//      }])
    //    (
    //      Subset.consSubset(lsi, Element.consElement(Element.nilElement), Subset.nilSubset)
    //    )
    //      Subset.cons[Reader_[String] :+: Reader_[Int] :+: CNil, Reader_[Int], CNil](
    //        SubsetHelper.help    
    //      )
    //    )
  }

}