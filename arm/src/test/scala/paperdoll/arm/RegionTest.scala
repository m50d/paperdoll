package paperdoll.arm

import org.junit.Test
import scalaz.syntax.monad._
import Region._
import java.io.InputStreamReader
import shapeless.{:+:, CNil}
import java.io.InputStream
import paperdoll.writer.Writer_
import paperdoll.writer.Writer.tell
import shapeless.Nat._0
import shapeless.Nat._1
import java.io.BufferedReader
import paperdoll.core.effect.Eff

class RegionTest {
  
  /**
   * This could possibly be implemented using whileM or
   * untilM or similar but I can't quite see how.
   */
  private[this] def readToWriter(br: BufferedReader): Eff.One[Writer_[String], Unit] =
    Option(br.readLine()).fold({}.point[Eff.One_[Writer_[String]]#O])({
    line => tell(line) *> readToWriter(br)
})
  
	@Test def regionWithAccessInside(): Unit = {
	  type EffectStack = Region_[_0, InputStream] :+: Region_[_1, BufferedReader] :+: Writer_[String] :+: CNil
	  
	  val eff = for {
	    inputStream <- newSHandle(_0, getClass.getResourceAsStream("names.txt")).extend[EffectStack]()
	    reader <- newSHandle(_1, new BufferedReader(new InputStreamReader(inputStream, "UTF-8"))).extend[EffectStack]()
	    _ <- readToWriter(reader).extend[EffectStack]()
	  } yield {}
	}
}