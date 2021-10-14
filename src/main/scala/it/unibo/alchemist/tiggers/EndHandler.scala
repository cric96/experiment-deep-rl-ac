package it.unibo.alchemist.tiggers

import it.unibo.alchemist.boundary.interfaces.OutputMonitor
import it.unibo.alchemist.model.interfaces._

class EndHandler[T](logic: () => Unit) extends OutputMonitor[T, Position[_]] {
  override def finished(environment: Environment[T, Position[_]], time: Time, step: Long): Unit = logic()

  override def initialized(environment: Environment[T, Position[_]]): Unit = {}

  override def stepDone(
      environment: Environment[T, Position[_]],
      reaction: Reaction[T],
      time: Time,
      step: Long
  ): Unit = {}
}
