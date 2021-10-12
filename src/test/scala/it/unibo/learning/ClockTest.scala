package it.unibo.learning

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ClockTest extends AnyFlatSpec with should.Matchers {
  "A clock" should "start with zero" in {
    Clock.start.ticks shouldBe 0
  }

  "A clock" should "update only by one tick" in {
    Clock.start.tick shouldBe Clock(1)
  }
}
