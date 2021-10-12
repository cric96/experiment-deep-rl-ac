package it.unibo.scafi.casestudy

import it.unibo.learning.Q
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LearningProcessTest extends AnyFlatSpec with should.Matchers {
  private val aData = 10
  "A learning process step build" should "build a learning context" in {
    val q: Q[Int, Int] = Q.zeros()
    val state: Int => Int = data => data + 1
    val reward: Int => Double = _ => 30
    val actionEffect: (Int, Int) => Int = (left, right) => left + right
    val initialState = 0
    val initialOutput = 0
    val ctx = LearningProcess
      .QBuilderStep[Int, Int, Int](q)
      .stateDefinition(state)
      .rewardDefinition(reward)
      .actionEffectDefinition(actionEffect)
      .initialCondition(initialState, initialOutput)
    assert(
      ctx.q == q &&
        ctx.rewardSignal == reward &&
        ctx.actionEffect == actionEffect &&
        ctx.initialCondition.output == initialOutput &&
        ctx.initialCondition.state == initialState
    )
  }
}
