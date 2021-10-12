package it.unibo.learning

import cats.data
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class PolicyTest extends AnyFlatSpec with should.Matchers {
  private val seed = 42
  private implicit val random: Random = new Random(seed)
  private val actions = data.NonEmptySet.of(1, 2, 3)
  private val bigNumber = 100
  private val state = 0

  "A greedy policy" should "be used from a Q table" in {
    val q = Q.zeros[Int, Int]()
    val best = 2
    val withValues = q.update(0, 1, 1).update(0, best, 2).update(0, 3,-1)
    val bestAction = Policy.greedy(withValues, state, actions)
    bestAction shouldBe best
  }

  "A random policy" should "return actions randomly" in {
    LazyList.fill(bigNumber) { Policy.random(actions) }.toSet shouldBe actions
  }

  "An epsilon greedy policy" should "return actions randomly when epsilon is one" in {
    val epsilonList = LazyList.fill(bigNumber) {
      Policy.epsilonGreedy(Q.zeros[Int, Int](), state, actions, epsilon = 1)
    }.toSet

    epsilonList shouldBe actions
  }

  "An epsilon greedy policy" should "return greedy actions with epsilon equals to zero" in {
    val q = Q.zeros[Int, Int]()
    val best = 1
    val withValues = q.update(0, best, 1)
    val bestAction = Policy.epsilonGreedy(withValues, state, actions, epsilon = 0)
    bestAction shouldBe best
  }

}
