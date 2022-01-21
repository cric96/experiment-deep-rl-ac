package it.unibo.learning

import cats.data.NonEmptySet
import it.unibo.learning.ReinforcementLearning.Ops

/** A sars like RL learning process. The trajectory in this case will be SARS (state t, action t, reward t and state t
  * plus)
  */
object Sars {
  trait Type[S, A, T] extends ReinforcementLearning[(S, A, Double, S), T] {
    val ops: Ops[S, A, T]
    def actions: NonEmptySet[A]
  }
}
