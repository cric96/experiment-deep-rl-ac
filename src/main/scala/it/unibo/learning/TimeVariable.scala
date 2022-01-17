package it.unibo.learning

import scala.language.implicitConversions

sealed trait TimeVariable[A] {
  def value(clock: Episode): A
}

object TimeVariable {
  def independent[A](v: A): TimeVariable[A] = new TimeVariable[A] {
    override def value(clock: Episode): A = v
  }
  def follow[A](logic: Episode => A): TimeVariable[A] = new TimeVariable[A] {
    override def value(clock: Episode): A = logic(clock)
  }
  def exponentialDecayFunction(startBy: Double, factor: Double): TimeVariable[Double] =
    new TimeVariable[Double] {
      override def value(clock: Episode): Double = startBy * math.exp(-(clock.count / factor))
    }
  implicit def varToTimeVar[A](a: A): TimeVariable[A] = TimeVariable.independent(a)
}
