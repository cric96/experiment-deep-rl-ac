package it.unibo.learning

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EpisodeTest extends AnyFlatSpec with should.Matchers {
  "A episode" should "start with zero" in {
    Episode.zero.count shouldBe 0
  }

  "A episode" should "update only by one tick" in {
    Episode.zero.tick shouldBe Episode(1)
  }
}
