package it.unibo.learning

import cats.data.NonEmptySet
import it.unibo.cats.TypeEnrichment.RichNonEmptyList
object QLearning {
  trait Type[S, A] extends ReinforcementLearning[(S, A, Double, S), Q[S, A]] {
    def actions: NonEmptySet[A]
  }
  case class Plain[S, A](actions: NonEmptySet[A], alpha: TimeVariable[Double], gamma: Double)
      extends QLearning.Type[S, A] {
    override def improve(trajectory: (S, A, Double, S), q: Q[S, A], clock: Clock): Q[S, A] = {
      val (stateT, actionT, rewardTPlus, stateTPlus) = trajectory
      val oldQValue = q(stateT, actionT)
      val (_, bestNextValue) =
        actions.toNonEmptyList.map(action => (action, q(stateTPlus, action))).maxBy { case (_, value) => value }
      val update = oldQValue + alpha.value(clock) * rewardTPlus + gamma * bestNextValue - oldQValue
      q.update(stateT, actionT, update)
    }
  }
  class Hysteretic[S, A](
      val actions: NonEmptySet[A],
      val alpha: TimeVariable[Double],
      val beta: TimeVariable[Double],
      val gamma: Double
  ) extends QLearning.Type[S, A] {
    override def improve(trajectory: (S, A, Double, S), q: Q[S, A], clock: Clock): Q[S, A] = {
      val (stateT, actionT, rewardTPlus, stateTPlus) = trajectory
      val oldQValue = q(stateT, actionT)
      val (_, bestNextValue) =
        actions.toNonEmptyList.map(action => (action, q(stateTPlus, action))).maxBy { case (_, value) => value }

      val diff = rewardTPlus + gamma * bestNextValue - oldQValue
      val hystereticUpdate = if (diff >= 0) {
        alpha.value(clock) * diff
      } else {
        (beta.value(clock)) * diff
      }
      val update = oldQValue + hystereticUpdate
      q.update(stateT, actionT, update)
    }
  }

  object Hysteretic {
    def apply[S, A](
        actions: NonEmptySet[A],
        alpha: TimeVariable[Double],
        beta: TimeVariable[Double],
        gamma: Double
    ): QLearning.Hysteretic[S, A] = new Hysteretic[S, A](actions, alpha, beta, gamma)
  }

  class Distributed[S, A](actions: NonEmptySet[A], alpha: TimeVariable[Double], gamma: Double)
      extends Hysteretic[S, A](actions, alpha, TimeVariable.independent(1), gamma)

  object Distributed {
    def apply[S, A](actions: NonEmptySet[A], alpha: TimeVariable[Double], gamma: Double): Distributed[S, A] =
      new Distributed[S, A](actions, alpha, gamma)
  }
}
