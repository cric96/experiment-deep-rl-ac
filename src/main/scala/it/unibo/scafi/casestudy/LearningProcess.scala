package it.unibo.scafi.casestudy

import it.unibo.learning.{Clock, Q, QLearning, TimeVariable}

object LearningProcess {

  case class QBuilderTest[S, A, O](q: Q[S, A]) {
    def stateDefinition(state: O => S): RewardDefinition[S, A, O] = RewardDefinition(q, state)
  }

  case class RewardDefinition[S, A, O](
      q: Q[S, A],
      statePolicy: O => S
  ) {
    def rewardDefinition(rewardSignal: O => Double): ActionEffect[S, A, O] = ActionEffect(q, statePolicy, rewardSignal)
  }

  case class ActionEffect[S, A, O](
      q: Q[S, A],
      statePolicy: O => S,
      rewardSignal: O => Double
  ) {
    def actionEffectDefinition(action: (O, A) => O): LearningContext[S, A, O] =
      LearningContext(q, statePolicy, rewardSignal, action)
  }

  case class LearningContext[S, A, O](
      q: Q[S, A],
      statePolicy: O => S,
      rewardSignal: O => Double,
      actionEffect: (O, A) => O
  )

  trait BuilderFinalizer[S, A, O] {
    def learn(qLearning: QLearning[S, A], epsilon: TimeVariable[Double], initialClock: Clock): (Q[S, A], O)
    def act(): (Q[S, A], O)
  }
}
