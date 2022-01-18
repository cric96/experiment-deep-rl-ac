package it.unibo.learning

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TimeVariableTest extends AnyFlatSpec with should.Matchers {
  private val aData = 10
  def acceptVar[A](a: TimeVariable[A]): A = a.value(Episode.zero)

  "A time variable" should "be time independent" in {
    val clock = Episode.zero
    val invariant = TimeVariable.independent(aData)
    invariant.value(clock) shouldBe invariant.value(clock.tick)
  }

  "A time dependent variable" should "be influenced by the clock" in {
    val clock = Episode.zero
    val dependent = TimeVariable.follow(clock => clock.count)
    assert(dependent.value(clock) != dependent.value(clock.tick))
  }

  "A time dependent variable with exponential decay" should "be influenced by the clock" in {
    val clock = Episode.zero
    val dependent = TimeVariable.exponentialDecayFunction(aData, factor = 1)
    assert(dependent.value(clock) != dependent.value(clock.tick))
  }

  "A data" should "be converted in time variable implicitly" in {
    val data = acceptVar(aData)
    aData shouldBe data
  }
}
