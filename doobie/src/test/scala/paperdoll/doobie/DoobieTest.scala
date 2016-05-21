package paperdoll.doobie

import _root_.doobie.imports._
import paperdoll.scalaz.concurrent.Task_
import paperdoll.core.effect.Effects
import scalaz.Catchable
import scalaz.concurrent.Task
import paperdoll.core.effect.Effects.{ sendU, unsafeRun }
import org.junit.Test
import org.fest.assertions.Assertions.assertThat

class DoobieTest {
  implicit object catchable extends Catchable[Effects.One_[Task_]#O] {
    override def attempt[A](f: Effects.One[Task_, A]) = sendU(Task.taskInstance.attempt(unsafeRun(f)))
    override def fail[A](err: Throwable) = sendU(Task.taskInstance.fail[A](err))
  }
  implicit object capture extends Capture[Effects.One_[Task_]#O] {
    override def apply[A](a: ⇒ A) = sendU(Task.delay(a))
  }

  @Test def basicFunctionality(): Unit = {
    val xa = DriverManagerTransactor[Effects.One_[Task_]#O](
      "org.h2.Driver", "jdbc:h2:mem", "", "sa")

    val program = sql"select 42".query[Int].unique
    //TODO: translate as process, run using sendP
    val effect = xa.trans(program)
    val task = unsafeRun(effect)
    
    assertThat(task.unsafePerformSync).isEqualTo(42)
  }
}