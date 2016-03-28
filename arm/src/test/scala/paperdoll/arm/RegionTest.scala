package paperdoll.arm

import org.junit.Test
import scalaz.syntax.monad._
import Region._
import java.io.InputStreamReader
import shapeless.{:+:, CNil}
import java.io.InputStream
import paperdoll.writer.Writer_

class RegionTest {
	@Test def regionWithAccessInside(): Unit = {
	  type EffectStack = Region_[S, InputStream] :+: Region_[T, InputStreamReader] :+: Writer_[String] :+: CNil forSome {
	    type S; type T
	  }
	  
	  val eff = for {
	    inputStream <- newSHandle(getClass.getResourceAsStream("names.txt")).extend[EffectStack]()
	    reader <- newSHandle(new InputStreamReader(inputStream, "UTF-8")).extend[EffectStack]()
	  } yield {}
	}
}