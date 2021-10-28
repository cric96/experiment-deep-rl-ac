package it.unibo.learning

import cats.data.NonEmptySet
import it.unibo.learning.ReinforcementLearning.Ops

import scala.util.Random

object MonteCarlo {
  type Target[S, A] = (Q[S, A], Map[(S, A), List[Double]])
  trait Type[S, A] extends ReinforcementLearning[Seq[(S, A, Double)], Target[S, A]] {
    def actions: NonEmptySet[A]
    val ops: Ops[S, A, Target[S, A]] = new Ops[S, A, Target[S, A]] {
      override def extractQFromTarget(target: (Q[S, A], Map[(S, A), List[Double]])): Q[S, A] = target._1

      override def initTargetFromQ(q: Q[S, A]): (Q[S, A], Map[(S, A), List[Double]]) = (q, Map.empty)
    }
  }

  case class FirstVisit[S, A](actions: NonEmptySet[A], gamma: Double) extends Type[S, A] {
    override def improve(trajectory: Seq[(S, A, Double)], target: (Q[S, A], Map[(S, A), List[Double]]), clock: Clock)(
        implicit rand: Random
    ): (Q[S, A], Map[(S, A), List[Double]]) = {
      val (q, oldEstimation) = target
      val returns = trajectory.scanLeft(0.0) { case (g, (s, a, r)) => gamma * g + r }
      val (qUpdated, _, estimation) =
        (trajectory.zip(returns)).reverse.foldLeft((q, Set.empty[(S, A)], oldEstimation)) {
          case (acc @ (q, visited, estimation), ((s, a, _), g)) =>
            if (!visited.contains(s, a)) {
              val updateMap = estimation.updated((s, a), g :: estimation.getOrElse((s, a), List.empty))
              val updateQValue = updateMap.get((s, a)).map(l => l.sum / l.size)
              val updateQ = q.update(s, a, updateQValue.getOrElse(q(s, a)))
              (updateQ, visited.+((s, a)), updateMap)
            } else {
              acc
            }
        }
      (qUpdated, estimation)
    }
  }
}
