package it.unibo.learning

import cats.Reducible
import cats.data.{NonEmptyList, NonEmptySet}
import it.unibo.cats.TypeEnrichment._
import scala.util.Random

object Policy {
  type Type[S, C, A] = (S, C, Clock) => A
  type QBased[S, A] = Type[S, Q[S, A], A]

  def random[S, A](actions: NonEmptySet[A])(implicit rnd: Random): QBased[S, A] = (_, _, _) => {
    def randomFromList(actions: NonEmptyList[A]): A = actions match {
      case NonEmptyList(head, Nil)              => head
      case NonEmptyList(head, nextHead :: tail) => Stochastic.flip(head, randomFromList(NonEmptyList(nextHead, tail)))
    }
    randomFromList(Reducible[NonEmptySet].toNonEmptyList(actions))
  }

  def greedy[S, A](actions: NonEmptySet[A]): QBased[S, A] = (state, q, _) => {
    val (action, _) = actions.toNonEmptyList
      .map(action => (action, q(state, action)))
      .maxBy(_._2)
    action
  }

  def epsilonGreedy[S, A](actions: NonEmptySet[A], epsilonT: TimeVariable[Double])(implicit rnd: Random): QBased[S, A] =
    (state, q, clock) => {
      val epsilon = epsilonT.value(clock)
      require(epsilon >= 0 && epsilon <= 1)
      val policy = if (rnd.nextDouble() <= epsilon) { random[S, A](actions) }
      else { greedy[S, A](actions) }
      policy(state, q, clock)
    }

  def softEpsilonGreedy[S, A](actions: NonEmptySet[A], epsilonT: TimeVariable[Double])(implicit
      rnd: Random
  ): QBased[S, A] = {
    val actionSize = actions.length
    (state, q, clock) =>
      val epsilon = epsilonT.value(clock)
      val greedy = Policy.greedy(actions)(state, q, clock)
      val other = actions.filter(_ != greedy).toList
      val bestProb = (1 - epsilon) + epsilon / actionSize
      val otherProb = epsilon / actionSize
      val actionsWithProb =
        other.map(action => (action, otherProb)).scan((greedy, bestProb)) { case ((_, accProb), (action, prob)) =>
          (action, prob + accProb)
        }
      Stochastic.sampleFrom(NonEmptyList.fromListUnsafe(actionsWithProb))
  }

  def softFixedEpsilonGreedy[S, A](actions: NonEmptySet[A], epsilonT: Double)(implicit
      rnd: Random
  ): QBased[S, A] = softEpsilonGreedy(actions, TimeVariable.independent(epsilonT))
}
