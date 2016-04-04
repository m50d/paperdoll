package paperdoll.core.nondeterminism

import org.junit.Test
import NDet._
import scalaz.std.list._
import scalaz.syntax.monadPlus._
import org.fest.assertions.Assertions.assertThat
import scala.Vector
import paperdoll.core.effect.Eff
import scala.Predef.intWrapper

class NDetTest {
  @Test def testIfte(): Unit = {
    val gen = collapse((2 to 30).toList)
    val eff = for {
      n <- gen
      x <- ifte(for {
        d <- gen
        if(d < n && n % d == 0)
      } yield {}, {_: Unit => Zero[Int]}, n.point[Eff.One_[NDet_]#O])
    } yield n
    val _ = assertThat(runNDetVector(eff).run).isEqualTo(Vector(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
  }
}