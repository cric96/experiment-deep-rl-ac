package it.unibo.learning

import cats.data.NonEmptySet
import it.unibo.learning.ReinforcementLearning.Ops

import scala.util.Random

object MonteCarlo {
  type Target[S, A] = (Q[S, A], Map[(S, A), (Double, Int)])

  trait Type[S, A] extends ReinforcementLearning[Seq[(S, A, Double)], Target[S, A]] {
    def actions: NonEmptySet[A]

    val ops: Ops[S, A, Target[S, A]] = new Ops[S, A, Target[S, A]] {
      override def extractQFromTarget(target: (Q[S, A], Map[(S, A), (Double, Int)])): Q[S, A] = target._1

      override def initTargetFromQ(q: Q[S, A]): (Q[S, A], Map[(S, A), (Double, Int)]) = (q, Map.empty)
    }

    def improve(trajectory: Seq[(S, A, Double)], target: (Q[S, A], Map[(S, A), (Double, Int)]), clock: Clock)(implicit
        rand: Random
    ): (Q[S, A], Map[(S, A), (Double, Int)])
  }

  case class FirstVisit[S, A](actions: NonEmptySet[A], gamma: Double) extends Type[S, A] {
    override def improve(
        trajectory: Seq[(S, A, Double)],
        target: (Q[S, A], Map[(S, A), (Double, Int)]),
        clock: Clock
    )(implicit rand: Random): (Q[S, A], Map[(S, A), (Double, Int)]) = {
      val (q, oldEstimation) = target
      val returns = trajectory.scanLeft(0.0) { case (g, (s, a, r)) => gamma * g + r }
      val (qUpdated, _, estimation) =
        (trajectory.zip(returns)).reverse.foldLeft((q, Set.empty[(S, A)], oldEstimation)) {
          case (acc @ (q, visited, estimation), ((s, a, _), g)) =>
            if (!visited.contains(s, a)) {
              val (sum, count) = estimation.getOrElse((s, a), (0.0, 0))
              val updateMap = estimation.updated((s, a), ((g + sum), count + 1))
              val updateQValue = updateMap.get((s, a)).map { case (g, size) => g / size }
              val updateQ = q.update(s, a, updateQValue.getOrElse(q(s, a)))
              (updateQ, visited.+((s, a)), updateMap)
            } else {
              acc
            }
        }
      (qUpdated, estimation)
    }
  }

  case class EveryVisit[S, A](actions: NonEmptySet[A], gamma: Double) extends Type[S, A] {
    override def improve(trajectory: Seq[(S, A, Double)], target: (Q[S, A], Map[(S, A), (Double, Int)]), clock: Clock)(
        implicit rand: Random
    ): (Q[S, A], Map[(S, A), (Double, Int)]) = {
      val (q, oldEstimation) = target
      val returns = trajectory.scanLeft(0.0) { case (g, (_, _, r)) => gamma * g + r }
      (trajectory.zip(returns)).foldLeft((q, oldEstimation)) { case (acc @ (q, estimation), ((s, a, _), g)) =>
        val (sum, count) = estimation.getOrElse((s, a), (0.0, 0))
        val updateMap = estimation.updated((s, a), ((g + sum), count + 1))
        val updateQValue = updateMap.get((s, a)).map { case (g, size) => g / size }
        val updateQ = q.update(s, a, updateQValue.getOrElse(q(s, a)))
        (updateQ, updateMap)
      }
    }
  }
}
