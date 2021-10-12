package it.unibo.learning

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TimeVariableTest extends AnyFlatSpec with should.Matchers {
  def acceptVar[A](a: TimeVariable[A]): A = a.value(Clock.start)

  "A time variable" should "be time independent" in {
    val clock = Clock.start
    val invariant = TimeVariable.independent(10)
    invariant.value(clock) shouldBe invariant.value(clock.tick)
  }

  "A time dependent variable" should "be influenced by the clock" in {
    val clock = Clock.start
    val dependent = TimeVariable.follow(clock => clock.ticks)
    assert(dependent.value(clock) != dependent.value(clock.tick))
  }

  "A data" should "be converted in time variable implicitly" in {
    acceptVar(10)
    succeed
  }
}
