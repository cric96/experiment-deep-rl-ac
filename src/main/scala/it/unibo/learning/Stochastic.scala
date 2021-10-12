package it.unibo.learning

import scala.util.Random

object Stochastic {
  def flip[A](left: => A, right: => A)(implicit rnd: Random): A = flipUnbalanced(0.5)(left, right)
  def flipUnbalanced[A](prob: Double)(left: => A, right: => A)(implicit random: Random): A =
    if (random.nextDouble() < prob) left else right
}
