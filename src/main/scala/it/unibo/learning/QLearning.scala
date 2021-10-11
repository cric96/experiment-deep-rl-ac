package it.unibo.learning
case class QLearning[S, A](q: Q[S, A], epsilon: Double, alpha: Double) extends ReinforcementLearning[(S, A, Double, S), Q[S, A]] {
  override def improve(t: (S, A, Double, S)): Q[S, A] = {
    val (stateT, actionT, rewardTPlus, stateTPlus) = t
    q
  }
}
