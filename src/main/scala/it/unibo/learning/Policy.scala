package it.unibo.learning

import cats.Reducible
import cats.data.{NonEmptyList, NonEmptySet}
import it.unibo.cats.TypeEnrichment._
import scala.util.Random

object Policy {
  def random[S, A](actions: NonEmptySet[A])(implicit rnd: Random): A = {
    def randomFromList(actions: NonEmptyList[A]): A = actions match {
      case NonEmptyList(head, Nil) => head
      case NonEmptyList(head, nextHead :: tail) => Stochastic.flip(head, randomFromList(NonEmptyList(nextHead, tail)))
    }
    randomFromList(Reducible[NonEmptySet].toNonEmptyList(actions))
  }
  def greedy[S, A](q: Q[S, A], state: S, actions: NonEmptySet[A]): A = {
    val (action, _) = actions.toNonEmptyList
      .map(action => (action, q(state, action)))
      .maxBy(_._2)
    action
  }
  def epsilonGreedy[S, A](q: Q[S, A], state: S, actions: NonEmptySet[A], epsilon: Double)(implicit rnd: Random): A = {
    require(epsilon >= 0 && epsilon <= 1)
    if(rnd.nextDouble() <= epsilon) { random(actions) } else { greedy(q, state, actions) }
  }
}
