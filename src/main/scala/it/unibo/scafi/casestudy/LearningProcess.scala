package it.unibo.scafi.casestudy

import it.unibo.learning._

import scala.util.Random

object LearningProcess {
  case class InitialCondition[S, O](state: S, output: O)
  case class RoundData[S, A, O](q: Q[S, A], output: O, action: A, clock: Clock)
  type Trajectory[S, A] = Seq[(S, A, Double)]
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
    def initialConditionDefinition(initialState: S, initialOutput: O): LearningContext[S, A, O] =
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
    def learn[T](learning: Sars.Type[S, A, T], epsilon: TimeVariable[Double], clock: Clock)(implicit
        rnd: Random
    ): (RoundData[S, A, O], Trajectory[S, A])
    def act[T](learning: Sars.Type[S, A, T], clock: Clock)(implicit
        rand: Random
    ): (RoundData[S, A, O], Trajectory[S, A])
  }
}
