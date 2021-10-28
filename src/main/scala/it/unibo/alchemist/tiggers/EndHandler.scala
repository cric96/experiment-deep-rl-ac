package it.unibo.alchemist.tiggers

import it.unibo.alchemist.boundary.interfaces.OutputMonitor
import it.unibo.alchemist.model.interfaces._

class EndHandler[T](sharedLogic: () => Unit, leaderLogic: () => Unit, id: Int) extends OutputMonitor[T, Position[_]] {
  override def finished(environment: Environment[T, Position[_]], time: Time, step: Long): Unit = {
    if (id == 0) { leaderLogic() }
    sharedLogic()
  }

  override def initialized(environment: Environment[T, Position[_]]): Unit = {}

  override def stepDone(
      environment: Environment[T, Position[_]],
      reaction: Reaction[T],
      time: Time,
      step: Long
  ): Unit = {}
}
