package it.unibo.learning

trait ReinforcementLearning[Trajectory, Target] {
  def improve(t: Trajectory): Target
}
