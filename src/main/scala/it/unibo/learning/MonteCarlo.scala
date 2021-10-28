package it.unibo.learning

import cats.data.NonEmptySet

import scala.util.Random

object MonteCarlo {
  trait Type[S, A] extends ReinforcementLearning[Seq[(S, A, Double)], (Q[S, A], Policy.Type[S, Q[S, A], A])] {
    def actions: NonEmptySet[A]
    def states: NonEmptySet[S]
  }

  case class FirstVisit[S, A](actions: NonEmptySet[A], states: NonEmptySet[S]) extends Type[S, A] {
    override def improve(trajectory: Seq[(S, A, Double)], target: (Q[S, A], Policy.Type[S, Q[S, A], A]), clock: Clock)(
        implicit rand: Random
    ): (Q[S, A], Policy.Type[S, Q[S, A], A]) = ???
  }
}
