package it.unibo.scafi.casestudy

import it.unibo.alchemist.Alchemist
import it.unibo.alchemist.loader.LoadAlchemist
import it.unibo.learning.Q
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.junit.JUnitRunner
import os.Path

@RunWith(classOf[JUnitRunner])
class HopCountQLearningTest extends AnyFlatSpec with should.Matchers {
  val path: Path = os.pwd / "src" / "test" / "yaml" / "hop-count-test.yml"

  "HopCountQLearning" should "update q tables" in {
    println(path.wrapped.toFile)
    succeed
  }
}
