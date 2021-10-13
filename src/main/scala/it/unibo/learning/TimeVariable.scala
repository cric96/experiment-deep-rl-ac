package it.unibo.learning

import scala.language.implicitConversions

sealed trait TimeVariable[A] {
  def value(clock: Clock): A
}

object TimeVariable {
  def independent[A](v: A): TimeVariable[A] = new TimeVariable[A] {
    override def value(clock: Clock): A = v
  }
  def follow[A](logic: Clock => A): TimeVariable[A] = new TimeVariable[A] {
    override def value(clock: Clock): A = logic(clock)
  }
  def fromFunc[A](logic: java.util.function.Function[Long, A]): TimeVariable[A] =
    follow((clock: Clock) => logic.apply(clock.ticks))
  implicit def varToTimeVar[A](a: A): TimeVariable[A] = TimeVariable.independent(a)
}
