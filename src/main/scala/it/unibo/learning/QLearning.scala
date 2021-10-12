package it.unibo.learning

import cats.data.NonEmptySet
import it.unibo.cats.TypeEnrichment.RichNonEmptyList

case class QLearning[S, A](actions: NonEmptySet[A], alpha: TimeVariable[Double], gamma: Double)
    extends ReinforcementLearning[(S, A, Double, S), Q[S, A]] {
  override def improve(trajectory: (S, A, Double, S), q: Q[S, A], clock: Clock): Q[S, A] = {
    val (stateT, actionT, rewardTPlus, stateTPlus) = trajectory
    val oldQValue = q(stateT, actionT)
    val (_, bestNextValue) =
      actions.toNonEmptyList.map(action => (action, q(stateTPlus, action))).maxBy { case (_, value) => value }
    val update = oldQValue + alpha.value(clock) * (rewardTPlus + gamma * bestNextValue - oldQValue)
    q.update(stateT, actionT, update)
  }
}
