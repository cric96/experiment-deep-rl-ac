package it.unibo.learning

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class StochasticTest extends AnyFlatSpec with should.Matchers {
  private val seed = 42
  private implicit val random: Random = new Random(seed)
  private val bigNumber = 1000
  val (head, cross) = (0, 1)
  val lowProb = 0.1
  "Stochastic" should "flip a coin accordingly" in {
    val spins = List.fill(bigNumber){ Stochastic.flip(0, 1) }
    assert(spins.contains(head) && spins.contains(cross))
  }
  "Stochastic" should "flip a 'fake' coin accordingly" in {
    val spins = List.fill(bigNumber){ Stochastic.flipUnbalanced(lowProb)(0, 1) }
    assert(spins.count(_ == head) < spins.count(_ == cross))
  }

}
