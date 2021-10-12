package it.unibo.scafi.casestudy

import it.unibo.learning.{Clock, Q, QLearning, TimeVariable}

import scala.util.Random

object LearningProcess {
  case class InitialCondition[S, O](state: S, output: O)
  case class QBuilderStep[S, A, O](q: Q[S, A]) {
    def stateDefinition(state: O => S): RewardDefinitionStep[S, A, O] = RewardDefinitionStep(q, state)
  }

  case class RewardDefinitionStep[S, A, O](
      q: Q[S, A],
      statePolicy: O => S
  ) {
    def rewardDefinition(rewardSignal: O => Double): ActionEffectStep[S, A, O] =
      ActionEffectStep(q, statePolicy, rewardSignal)
  }

  case class ActionEffectStep[S, A, O](
      q: Q[S, A],
      statePolicy: O => S,
      rewardSignal: O => Double
  ) {
    def actionEffectDefinition(action: (O, A) => O): InitialConfigurationStep[S, A, O] =
      InitialConfigurationStep(q, statePolicy, rewardSignal, action)
  }

  case class InitialConfigurationStep[S, A, O](
      q: Q[S, A],
      statePolicy: O => S,
      rewardSignal: O => Double,
      actionEffect: (O, A) => O
  ) {
    def initialCondition(initialState: S, initialOutput: O): LearningContext[S, A, O] =
      LearningContext(q, statePolicy, rewardSignal, actionEffect, InitialCondition(initialState, initialOutput))
  }

  case class LearningContext[S, A, O](
      q: Q[S, A],
      statePolicy: O => S,
      rewardSignal: O => Double,
      actionEffect: (O, A) => O,
      initialCondition: InitialCondition[S, O]
  )

  trait BuilderFinalizer[S, A, O] {
    def learn(qLearning: QLearning[S, A], epsilon: TimeVariable[Double], initialClock: Clock)(implicit
        rnd: Random
    ): (Q[S, A], O)
    def act(q: QLearning[S, A])(implicit rand: Random): (Q[S, A], O)
  }
}
