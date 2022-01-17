package it.unibo.learning

import cats.data.NonEmptySet
import it.unibo.cats.TypeEnrichment.RichNonEmptyList
import it.unibo.learning.ReinforcementLearning.Ops

import scala.util.Random
object QLearning {
  private[QLearning] def max[S, A](q: Q[S, A], actions: NonEmptySet[A], stateTPlus: S)(implicit
      random: Random
  ): Double = {
    val (_, bestNextValue) =
      actions.toNonEmptyList.shuffle.map(action => (action, q(stateTPlus, action))).maxBy { case (_, value) => value }
    bestNextValue
  }

  trait Type[S, A] extends Sars.Type[S, A, Q[S, A]] {
    override val ops: Ops[S, A, Q[S, A]] = new Ops[S, A, Q[S, A]] {
      override def extractQFromTarget(target: Q[S, A]): Q[S, A] = target
      override def initTargetFromQ(q: Q[S, A]): Q[S, A] = q
    }
  }

  case class Plain[S, A](actions: NonEmptySet[A], alpha: Double, gamma: Double) extends QLearning.Type[S, A] {
    override def improve(trajectory: (S, A, Double, S), q: Q[S, A])(implicit
        random: Random
    ): Q[S, A] = {
      val (stateT, actionT, rewardTPlus, stateTPlus) = trajectory
      val oldQValue = q(stateT, actionT)
      val bestNextValue = max(q, actions, stateTPlus)
      val update = oldQValue + alpha * rewardTPlus + gamma * bestNextValue - oldQValue
      q.update(stateT, actionT, update)
    }

  }

  class Hysteretic[S, A](
      val actions: NonEmptySet[A],
      val alpha: Double,
      val beta: Double,
      val gamma: Double
  ) extends QLearning.Type[S, A] {
    override def improve(trajectory: (S, A, Double, S), q: Q[S, A])(implicit
        random: Random
    ): Q[S, A] = {
      val (stateT, actionT, rewardTPlus, stateTPlus) = trajectory
      val oldQValue = q(stateT, actionT)
      val bestNextValue = max(q, actions, stateTPlus)
      val diff = rewardTPlus + gamma * bestNextValue - oldQValue
      val learningFactor =
        if (diff >= 0) { alpha }
        else { beta }

      val hystereticUpdate = learningFactor * diff
      val update = oldQValue + hystereticUpdate
      q.update(stateT, actionT, update)
    }
  }

  object Hysteretic {
    def apply[S, A](
        actions: NonEmptySet[A],
        alpha: Double,
        beta: Double,
        gamma: Double
    ): QLearning.Hysteretic[S, A] = new Hysteretic[S, A](actions, alpha, beta, gamma)
  }
}
