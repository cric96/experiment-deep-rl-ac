package it.unibo.learning

import cats.Order
import cats.data.NonEmptySet
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class QLearningTest extends AnyFlatSpec with should.Matchers {
  sealed trait State
  case object StateA extends State
  case object StateB extends State
  sealed trait Action
  implicit val order: Order[Action] = Order.allEqual[Action]
  case object ActionA extends Action
  case object ActionB extends Action
  private val actions = NonEmptySet.of[Action](ActionA, ActionB)
  private val q = Q
    .zeros[State, Action]()
    .update(StateA, ActionA, 0)
    .update(StateA, ActionB, 1)
    .update(StateB, ActionA, 1)
    .update(StateB, ActionB, 3)
  private val currentState = StateA
  private val action = ActionA
  private val nextState = StateB
  private val gamma = 1
  private val variableTime = 0.5
  private val reward = 1
  implicit private val random = new Random()

  def testQLearning(process: QLearning.Type[State, Action], processName: String): Unit = {
    s"$processName process" should "update the Q table accordingly" in {
      val updatedQ = process.improve((currentState, action, reward, nextState), q)
      q(currentState, action) != updatedQ(currentState, action)
    }
  }

  testQLearning(QLearning.Plain(actions, variableTime, gamma), "Q Learning")
  testQLearning(QLearning.Hysteretic(actions, variableTime, variableTime, gamma), "Hysteretic Q Learning")
}
