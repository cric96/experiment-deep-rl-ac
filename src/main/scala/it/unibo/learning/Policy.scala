package it.unibo.learning

import cats.Reducible
import cats.data.{NonEmptyList, NonEmptySet}
import it.unibo.cats.TypeEnrichment._
import scala.util.Random

/** A RL policy. Given a state, it gives in output the action that the agent should perform */
object Policy {
  // general type: C is the context used by the policy to choose want action the agent will perform
  type Type[S, C, A] = (S, C) => A
  // the context is the q table. Typical policy. It cannot be used in deep learning algorithm for instance.
  type QBased[S, A] = Type[S, Q[S, A], A]

  // explorative policy.
  def random[S, A](actions: NonEmptySet[A])(implicit rnd: Random): QBased[S, A] = (_, _) => {
    def randomFromList(actions: NonEmptyList[A]): A = actions match {
      case NonEmptyList(head, Nil)              => head
      case NonEmptyList(head, nextHead :: tail) => Stochastic.flip(head, randomFromList(NonEmptyList(nextHead, tail)))
    }
    randomFromList(Reducible[NonEmptySet].toNonEmptyList(actions))
  }

  // the policy will choose always the action with the highest q value
  def greedy[S, A](actions: NonEmptySet[A]): QBased[S, A] = (state, q) => {
    val (action, _) = actions.toNonEmptyList
      .map(action => (action, q(state, action)))
      .maxBy(_._2)
    action
  }

  // eps probability to used an explorative policy, (1-eps) to use a greedy policy
  def epsilonGreedy[S, A](actions: NonEmptySet[A], epsilonT: Double)(implicit rnd: Random): QBased[S, A] =
    (state, q) => {
      val epsilon = epsilonT
      require(epsilon >= 0 && epsilon <= 1)
      val policy = if (rnd.nextDouble() <= epsilon) { random[S, A](actions) }
      else { greedy[S, A](actions) }
      policy(state, q)
    }

  // each action has at least e / N probability to be chosen
  def softEpsilonGreedy[S, A](actions: NonEmptySet[A], epsilonT: Double)(implicit
      rnd: Random
  ): QBased[S, A] = {
    val actionSize = actions.length
    (state, q) =>
      val epsilon = epsilonT
      val greedy = Policy.greedy(actions)(state, q)
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
  ): QBased[S, A] = softEpsilonGreedy(actions, epsilonT)
}
