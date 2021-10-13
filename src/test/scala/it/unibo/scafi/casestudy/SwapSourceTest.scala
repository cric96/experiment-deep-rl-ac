package it.unibo.scafi.casestudy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import os.Path
import it.unibo.learning.Q
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import it.unibo.AlchemistHelper._
import it.unibo.alchemist.core.implementations.Engine
import it.unibo.alchemist.model.interfaces.GeoPosition

@RunWith(classOf[JUnitRunner])
@SuppressWarnings(Array("org.wartremover.warts.Any")) // because of alchemist molecule management
class SwapSourceTest extends AnyFlatSpec with should.Matchers {
  val simulationPath: Path = os.pwd / "src" / "main" / "yml" / "swap-source.yml"
  "SwapSource" should "improve with learning" in {
    val engine: Engine[Any, GeoPosition] = loadAlchemist(simulationPath)
    engine.play()
    engine.run()
    engine.forEach { node =>
      assert(node.get[Q[List[Int], Int]]("qtable") != Q.zeros[List[Int], Int]())
    }
    succeed
  }
}
