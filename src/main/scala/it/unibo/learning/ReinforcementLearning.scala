package it.unibo.learning

import scala.util.Random

trait ReinforcementLearning[-Trajectory, Target] {
  def improve(trajectory: Trajectory, target: Target, clock: Clock)(implicit rand: Random): Target
}

object ReinforcementLearning {
  trait Ops[S, A, T] {
    type Aux = T
    def extractQFromTarget(target: Aux): Q[S, A]
    def initTargetFromQ(q: Q[S, A]): Aux
  }
}
