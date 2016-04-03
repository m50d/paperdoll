package paperdoll.nondeterminism

import org.junit.Test
import scala.Predef.intWrapper
import NDet._
import scalaz.syntax.foldable._
import scalaz.std.list._
import scalaz.syntax.monadPlus._
import paperdoll.core.effect.Eff

class NDetTest {
  @Test def testIfte(): Unit = {
    val gen = (2 to 30).toList.collapse[Eff.One_[NDet_]#O]
    val eff = for {
      n <- gen
      x <- ifte(for {
        d <- gen
        if(d < n && n % d == 0)
      } yield {}, {_: Unit => Zero[Int]}, n.point[Eff.One_[NDet_]#O])
    } yield n
  }
}