package paperdoll.nondeterminism

import org.junit.Test
import scala.Predef.intWrapper
import NDet._
import scalaz.syntax.foldable._
import scalaz.std.list._
import scalaz.syntax.monadPlus._
import paperdoll.core.effect.Eff

class NDetTest {
  @Test def ifte(): Unit = {
    val gen = (2 to 30).toList.collapse[Eff.One_[NDet_]#O]
    val eff = for {
      n <- gen
    } yield n
  }
}