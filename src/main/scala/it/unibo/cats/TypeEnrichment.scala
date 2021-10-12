package it.unibo.cats

import cats.data.NonEmptyList

object TypeEnrichment {
  implicit class RichNonEmptyList[A](nonEmpty: NonEmptyList[A]) {
    def maxBy[B : Numeric](select: A => B): A = nonEmpty.reduceLeft[A] {
      case(acc, data) => if(Numeric[B].gteq(select(acc), select(data))) { acc } else { data }
    }
  }
}
