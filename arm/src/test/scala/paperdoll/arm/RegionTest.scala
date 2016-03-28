package paperdoll.arm

import org.junit.Test
import scalaz.syntax.monad._
import Region._
import java.io.InputStreamReader
import shapeless.{:+:, CNil}
import java.io.InputStream
import paperdoll.writer.Writer_
import shapeless.Nat._0
import shapeless.Nat._1

class RegionTest {
	@Test def regionWithAccessInside(): Unit = {
	  type EffectStack = Region_[_0, InputStream] :+: Region_[_1, InputStreamReader] :+: Writer_[String] :+: CNil
	  
	  val eff = for {
	    inputStream <- newSHandle(_0, getClass.getResourceAsStream("names.txt")).extend[EffectStack]()
	    reader <- newSHandle(_1, new InputStreamReader(inputStream, "UTF-8")).extend[EffectStack]()
	  } yield {}
	}
}