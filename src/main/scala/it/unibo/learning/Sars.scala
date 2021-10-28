package it.unibo.learning

import cats.data.NonEmptySet

object Sars {
  trait Ops[S, A, T] {
    type Aux = T
    def extractQFromTarget(target: Aux): Q[S, A]
    def initTargetFromQ(q: Q[S, A]): Aux
  }
  trait Type[S, A, T] extends ReinforcementLearning[(S, A, Double, S), T] {
    val ops: Ops[S, A, T]
    def actions: NonEmptySet[A]
  }
}
