package it.unibo

import it.unibo.alchemist.core.implementations.Engine
import it.unibo.alchemist.loader.LoadAlchemist
import it.unibo.alchemist.model.implementations.nodes.{NodeManager, SimpleNodeManager}
import it.unibo.alchemist.model.interfaces.Position
import os.Path

object AlchemistHelper {
  implicit class RichEngine[A, P <: Position[P]](engine: Engine[A, P]) {
    def forEach(updateLogic: NodeManager => Unit): Unit =
      engine.getEnvironment.forEach(node => updateLogic(new SimpleNodeManager[A](node)))
  }

  def loadAlchemist[A, P <: Position[P]](file: Path): Engine[A, P] =
    new Engine(LoadAlchemist.from(file.wrapped.toFile).getDefault[A, P]().getEnvironment)
}
