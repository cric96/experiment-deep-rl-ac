package it.unibo.scafi.casestudy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import os.{Path, pwd}
import it.unibo.learning.Q
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import it.unibo.AlchemistHelper._
import it.unibo.alchemist.core.implementations.Engine
import it.unibo.alchemist.model.interfaces.GeoPosition
import it.unibo.storage.LocalStorage
import org.scalatest.BeforeAndAfterEach

@RunWith(classOf[JUnitRunner])
@SuppressWarnings(Array("org.wartremover.warts.Any")) // because of alchemist molecule management
class SwapSourceTest extends AnyFlatSpec with should.Matchers with BeforeAndAfterEach {
  val simulationPath: Path = os.pwd / "src" / "test" / "yaml" / "swapSourceHopCount.yml"
  val qTables = new LocalStorage[Int]("qtables_test")
  val clock = new LocalStorage[Int]("clock_test")
  private def clean(): Unit = {
    qTables.clean()
    clock.clean()
  }
  override def beforeEach(): Unit =
    clean()
  override def afterEach(): Unit =
    clean()

  "SwapSource" should "improve with learning" in {
    val engine: Engine[Any, GeoPosition] = loadAlchemist(simulationPath)
    engine.play()
    engine.run()
    engine.forEach { node =>
      assert(node.get[Q[List[Int], Int]]("qtable") != Q.fillWith[List[Int], Int](node.get[Double]("initial_value")))
    }
    engine.getEnvironment.forEach { node =>
      assert(os.exists(pwd / "qtables_test" / node.getId.toString))
    }
    succeed
  }

  "SwapSource" should "leverage q" in {
    val engine: Engine[Any, GeoPosition] = loadAlchemist(simulationPath)
    engine.forEach(node => node.put("learn", false))
    engine.play()
    engine.run()
    engine.forEach { node =>
      assert(node.get[Q[List[Int], Int]]("qtable") == Q.fillWith[List[Int], Int](node.get[Double]("initial_value")))
    }
    succeed
  }
}
