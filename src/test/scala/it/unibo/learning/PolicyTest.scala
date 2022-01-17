package it.unibo.learning

import cats.data
import it.unibo.learning.Policy.{QBased, epsilonGreedy}
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class PolicyTest extends AnyFlatSpec with should.Matchers {
  private val seed = 42
  implicit private val random: Random = new Random(seed)
  private val actions = data.NonEmptySet.of(1, 2, 3)
  private val bigNumber = 100
  private val state = 0
  val greedy: QBased[Int, Int] = Policy.greedy[Int, Int](actions)
  val randomPolicy: QBased[Int, Int] = Policy.random[Int, Int](actions)
  val epsilonGreedy: QBased[Int, Int] = Policy.epsilonGreedy[Int, Int](actions, 1)
  val greedyByEpsilon: QBased[Int, Int] = Policy.epsilonGreedy[Int, Int](actions, 0)
  "A greedy policy" should "be used from a Q table" in {
    val q = Q.zeros[Int, Int]()
    val best = 2
    val withValues = q.update(0, 1, 1).update(0, best, 2).update(0, 3, -1)
    val bestAction = greedy(state, withValues)
    bestAction shouldBe best
  }

  "A random policy" should "return actions randomly" in {
    LazyList.fill(bigNumber)(randomPolicy(state, Q.zeros())).toSet shouldBe actions
  }

  "An epsilon greedy policy" should "return actions randomly when epsilon is one" in {
    val epsilonList = LazyList
      .fill(bigNumber) {
        epsilonGreedy(state, Q.zeros())
      }
      .toSet
    epsilonList shouldBe actions
  }

  "An epsilon greedy policy" should "return greedy actions with epsilon equals to zero" in {
    val q = Q.zeros[Int, Int]()
    val best = 1
    val withValues = q.update(0, best, 1)
    val bestAction = greedyByEpsilon(state, withValues)
    bestAction shouldBe best
  }

}
