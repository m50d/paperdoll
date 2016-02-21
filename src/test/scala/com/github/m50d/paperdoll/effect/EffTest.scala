package com.github.m50d.paperdoll.effect

import org.junit.Test
import shapeless.{CNil, :+:}
import com.github.m50d.paperdoll.reader.Reader_

class EffTest {
	@Test def alignment(): Unit = {
	  type MyLayers = Reader_[String] :+: Reader_[Int] :+: CNil
	  
	  val x: MyLayers = null
//	  x
	}
}