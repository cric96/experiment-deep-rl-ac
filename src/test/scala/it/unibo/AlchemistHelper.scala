package it.unibo

import it.unibo.alchemist.core.implementations.Engine
import it.unibo.alchemist.loader.LoadAlchemist
import it.unibo.alchemist.model.implementations.nodes.{NodeManager, SimpleNodeManager}
import it.unibo.alchemist.model.implementations.times.DoubleTime
import it.unibo.alchemist.model.interfaces.Position
import os.Path

object AlchemistHelper {
  implicit class RichEngine[A, P <: Position[P]](engine: Engine[A, P]) {
    def forEach(updateLogic: NodeManager => Unit): Unit =
      engine.getEnvironment.forEach(node => updateLogic(new SimpleNodeManager[A](node)))
  }
  def loadAlchemistLimitedAt[A, P <: Position[P]](file: Path, runningTime: Long): Engine[A, P] =
    new Engine[A, P](
      LoadAlchemist.from(file.wrapped.toFile).getDefault[A, P].getEnvironment,
      new DoubleTime(runningTime.toDouble)
    )
  def loadAlchemist[A, P <: Position[P]](file: Path): Engine[A, P] =
    new Engine[A, P](LoadAlchemist.from(file.wrapped.toFile).getDefault[A, P]().getEnvironment)
}
