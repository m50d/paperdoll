package paperdoll.nondeterminism

import org.junit.Test
import scala.Predef.intWrapper
import NDet._
import scalaz.std.list._
import scalaz.syntax.monadPlus._
import paperdoll.core.effect.Eff
import org.fest.assertions.Assertions.assertThat

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