package it.unibo.learning

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ClockTest extends AnyFlatSpec with should.Matchers {
  "A clock" should "start with zero" in {
    Episode.start.count shouldBe 0
  }

  "A clock" should "update only by one tick" in {
    Episode.start.tick shouldBe Episode(1)
  }
}
