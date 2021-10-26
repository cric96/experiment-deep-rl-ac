package it.unibo.learning

import cats.data.NonEmptySet

object Sars {
  trait Type[S, A, T] extends ReinforcementLearning[(S, A, Double, S), T] {
    type Aux = T
    def extractQFromTarget(target: Aux): Q[S, A]
    def initTargetFromQ(q: Q[S, A]): Aux
    def actions: NonEmptySet[A]
  }
}
