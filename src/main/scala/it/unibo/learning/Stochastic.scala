package it.unibo.learning

import cats.data.NonEmptyList

import scala.util.Random

object Stochastic {
  def flip[A](left: => A, right: => A)(implicit rnd: Random): A = flipUnbalanced(0.5)(left, right)
  def flipUnbalanced[A](prob: Double)(left: => A, right: => A)(implicit random: Random): A =
    if (isHead(prob)) { left }
    else { right }

  def isHead(prob: Double)(implicit random: Random): Boolean = random.nextDouble() < prob

  def sampleFrom[A](iterable: NonEmptyList[(A, Double)])(implicit random: Random): A = {
    val prob = random.nextDouble()
    iterable.find(_._2 > prob).getOrElse(iterable.head)._1
  }
}
