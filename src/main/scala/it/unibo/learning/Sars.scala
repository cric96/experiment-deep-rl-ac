package it.unibo.learning

import cats.data.NonEmptySet
import it.unibo.learning.ReinforcementLearning.Ops

object Sars {
  trait Type[S, A, T] extends ReinforcementLearning[(S, A, Double, S), T] {
    val ops: Ops[S, A, T]
    def actions: NonEmptySet[A]
  }
}
