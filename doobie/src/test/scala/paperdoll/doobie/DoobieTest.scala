package paperdoll.doobie

import _root_.doobie.imports._
import paperdoll.scalaz.concurrent.Task_
import paperdoll.core.effect.Effects
import scalaz.Catchable
import scalaz.concurrent.Task

class DoobieTest {
  val xa = DriverManagerTransactor[Task](
  "org.h2.Driver", "jdbc:h2:mem", "", "sa"
)
}