package it.unibo.cats

import cats.data.NonEmptyList
import scala.util.Random

object TypeEnrichment {
  implicit class RichNonEmptyList[A](nonEmpty: NonEmptyList[A]) {
    def maxBy[B: Numeric](select: A => B): A = nonEmpty.reduceLeft[A] { case (acc, data) =>
      if (Numeric[B].gteq(select(acc), select(data))) { acc }
      else { data }
    }
    def shuffle(implicit random: Random): NonEmptyList[A] = {
      val listRep = random.shuffle(nonEmpty.toList)
      NonEmptyList.fromListUnsafe(listRep) // Safe because of starting from nonEmptyList
    }
  }
}
