package it.unibo.learning

import scala.util.Random

trait ReinforcementLearning[Trajectory, Target] {
  def improve(trajectory: Trajectory, target: Target, clock: Clock)(implicit rand: Random): Target
}
