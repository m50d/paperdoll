package paperdoll.doobie

import _root_.doobie.imports._
import paperdoll.scalaz.concurrent.Task_
import paperdoll.core.effect.Effects
import scalaz.Catchable
import scalaz.concurrent.Task

class DoobieTest {
 implicit def ca(implicit cat: Capture[Task]) = new Capture[Effects.One_[Task_]#O] {
   
 }
  
  val xa = DriverManagerTransactor[Effects.One_[Task_]#O](
  "org.h2.Driver", "jdbc:h2:mem", "", "sa"
)
}