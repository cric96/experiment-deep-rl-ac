package it.unibo.learning

import scala.util.Random

/** A (too) general reinforcement learning definition. It is a process where an agent will improve a target (a Q table?
  * a neural network?) following a trajectory( SARS? SARSA? an entire episode?).
  * @tparam Trajectory
  * @tparam Target
  */
trait ReinforcementLearning[-Trajectory, Target] {
  def improve(trajectory: Trajectory, target: Target)(implicit rand: Random): Target
}

object ReinforcementLearning {
  /** Utility to extract Q table from a reinforcement learning process
    * @tparam S
    * @tparam A
    * @tparam T
    */
  trait Ops[S, A, T] {
    type Aux = T
    def extractQFromTarget(target: Aux): Q[S, A]
    def initTargetFromQ(q: Q[S, A]): Aux
  }
}
