package it.unibo.learning

import scala.util.Random

object Policy {
  def random[S, A](actions: Set[A])(implicit rnd: Random): A = rnd.shuffle(actions.toList).head
  def greedy[S, A](q: Q[S, A], state: S, actions: Set[A]): A = {
    val (action, _) = actions
      .map(action => (action, q(state, action)))
      .maxBy(_._2)
    action
  }
  def epsilonGreedy[S, A](q: Q[S, A], state: S, actions: Set[A], epsilon: Double)(implicit rnd: Random): A = {
    require(epsilon >= 0 && epsilon <= 1)
    if(rnd.nextDouble() <= epsilon) { random(actions) } else { greedy(q, state, actions) }
  }
}
