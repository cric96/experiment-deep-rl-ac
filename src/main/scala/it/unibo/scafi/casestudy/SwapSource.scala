package it.unibo.scafi.casestudy

import cats.data.NonEmptySet
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.learning
import it.unibo.learning.{Clock, Q, QLearning, TimeVariable}
import it.unibo.storage.LocalStorage

import scala.util.Random

class SwapSource
    extends AggregateProgram
    with HopCountQLearning
    with StandardSensors
    with ScafiAlchemistSupport
    with Gradients {
  // Implicit context variable
  implicit lazy val rand: Random = randomGen
  // Storage
  lazy val qTableStorage = new LocalStorage[Int]("qtables")
  lazy val clockTableStorage = new LocalStorage[Int]("clock")
  lazy val passedTime: Double = alchemistTimestamp.toDouble
  // Variable loaded by alchemist configuration.
  lazy val leftSrc: Int = node.get[Integer]("left_source") // ID of the source at the left of the env (the stable one)
  lazy val rightSrc: Int =
    node.get[Integer]("right_source") // ID of the source at the right of the env (the unstable one)
  lazy val rightSrcStop: Int =
    node.get[Integer]("stop_right_source") // time at which the source at the right of the env stops being a source
  lazy val learnCondition: Boolean = node.get[java.lang.Boolean]("learn")
  lazy val initialValue: () => Double = node.get[() => Double]("initialValue")
  // Other constant
  lazy val windowDifferenceSize: Int = 3
  lazy val trajectorySize: Int = 7
  // Learning constants
  lazy val alpha: TimeVariable[Double] = node.get[learning.TimeVariable[Double]]("alpha")
  lazy val epsilon: TimeVariable[Double] = node.get[learning.TimeVariable[Double]]("epsilon")
  lazy val gamma: Double = node.get[java.lang.Double]("gamma")
  // Q Learning data
  lazy val actions: NonEmptySet[Int] = node.get[NonEmptySet[Int]]("actions")
  lazy val q: Q[List[Int], Int] = qTableStorage.loadOrElse(mid(), Q.fillWith(initialValue()))
  lazy val qLearning: QLearning[List[Int], Int] = QLearning(actions, alpha, gamma)
  // Source condition
  override def source: Boolean =
    if (mid() == leftSrc || (mid() == rightSrc && passedTime < rightSrcStop)) true else false
  // Aggregate Program data
  lazy val hopCountMetric: Metric = () => 1
  lazy val clock: Clock = clockTableStorage.loadOrElse(mid(), Clock.start)
  override def main(): Any = {
    val classicHopCount = classicGradient(source, hopCountMetric) // BASELINE
    val hopCountWithoutRightSource =
      classicGradient(mid() == leftSrc, hopCountMetric) // optimal gradient when RIGHT_SRC stops being a source
    val refHopCount = if (passedTime >= rightSrcStop) hopCountWithoutRightSource else classicHopCount
    // Learning definition
    val learningProblem = learningProcess(q)
      .stateDefinition(stateFromWindow)
      .rewardDefinition(output => rewardSignal(refHopCount.toInt, output))
      .actionEffectDefinition((output, action) => output + action)
      .initialConditionDefinition(List.empty, Double.PositiveInfinity)
    // RL Program execution
    val roundData = mux(learnCondition) {
      learningProblem.act(qLearning, Clock.start)
    } {
      learningProblem.learn(qLearning, epsilon, Clock.start)
    }
    // Store alchemist info

    // Store update data
    qTableStorage.save(mid(), roundData.q)
    clockTableStorage.save(mid(), roundData.clock)
  }

  private def stateFromWindow(output: Double): List[Int] = List.empty

  private def rewardSignal(groundTruth: Double, currentValue: Double): Double =
    if ((groundTruth - currentValue) == 0) { 0 }
    else { -1 }

}
