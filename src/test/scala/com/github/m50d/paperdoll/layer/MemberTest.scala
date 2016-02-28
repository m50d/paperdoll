package com.github.m50d.paperdoll.layer

import org.junit.Test
import shapeless.{CNil, :+:}
import com.github.m50d.paperdoll.reader.Reader_

class MemberTest {

  @Test def basicFunctionality(): Unit = {
    val l1 = Layers[Reader_[Int] :+: CNil]
    Member[Reader_[Int] :+: CNil, Layers.Aux[Reader_[Int] :+: CNil, l1.O], Reader_[Int]]
    
    val l2 = Layers[Reader_[String] :+: Reader_[Int] :+: CNil]
    Member[Reader_[String] :+: Reader_[Int] :+: CNil, Layers.Aux[Reader_[String] :+: Reader_[Int] :+: CNil, l2.O], Reader_[String]]
    //FIXME why doesn't it infer these types?
    Member[Reader_[String] :+: Reader_[Int] :+: CNil, Layers.Aux[Reader_[String] :+: Reader_[Int] :+: CNil, l2.O], Reader_[Int]](
    	Member.cons[Reader_[String], Reader_[Int] :+: CNil, Layers.Aux[Reader_[Int] :+: CNil, l1.O], Reader_[Int]]
    )
    
    
  }
}