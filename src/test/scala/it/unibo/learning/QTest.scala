package it.unibo.learning

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.junit.JUnitRunner
import scala.util.Random

@RunWith(classOf[JUnitRunner])
class QTest extends AnyFlatSpec with should.Matchers {
  private val random = new Random(42)
  private val longRandomSequence = (0 to 100).map(_ => (random.nextInt(), random.nextInt()))
  "A Q table" should "be created with 0 as default" in {
    val q = Q.zeros[Int, Int]()
    assert(longRandomSequence.map { case (state, action) => q(state, action) }.forall(_ == 0.0))
  }

  "A Q table" should "be updated" in {
    val rewardData = 10
    val q = Q.zeros[Int, Int]()
    val updated = q
      .update(state = 0, action = 0, reward = rewardData)
      .update(state = 0, action = 1, reward = rewardData)
    updated(0, 0) shouldBe rewardData
  }

  "A Q table" should "be filled with a default value" in {
    val rewardData = -10
    val q = Q.fillWith[Int, Int](rewardData)
    assert(longRandomSequence.map { case (state, action) => q(state, action) }.forall(_ == rewardData))
  }

  "A Q Table" should "change the default value" in {
    val q = Q.zeros[Int, Int]()
    assert(q(0, 0) == 0)
    val changeDefault = q.withDefault(10)
    changeDefault(0, 0) shouldBe 10
  }

}
