package it.unibo.learning

trait ReinforcementLearning[Trajectory, Target] {
  def improve(trajectory: Trajectory, target: Target, clock: Clock): Target
}
