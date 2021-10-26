package it.unibo.scafi.casestudy

import cats.data.NonEmptySet
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.alchemist.tiggers.EndHandler
import it.unibo.learning.{Clock, Q, QLearning, TimeVariable}
import it.unibo.storage.LocalStorage

import scala.util.Random

class SwapSource
    extends AggregateProgram
    with HopCountQLearning
    with StandardSensors
    with ScafiAlchemistSupport
    with BlockT
    with BlockG
    with FieldUtils
    with TemporalStateManagement
    with FixedGradients {
  // Type alias
  type State = List[Int]
  type Action = Int
  // Implicit context variable
  implicit lazy val rand: Random = randomGen
  // Storage
  lazy val qTableStorage = new LocalStorage[String](node.get[java.lang.String]("qtable_folder"))
  lazy val clockTableStorage = new LocalStorage[String](node.get[java.lang.String]("clock_folder"))
  def passedTime: Double = alchemistTimestamp.toDouble
  // Variable loaded by alchemist configuration.
  lazy val leftSrc: Int = node.get[Integer]("left_source") // ID of the source at the left of the env (the stable one)
  lazy val rightSrc: Int =
    node.get[Integer]("right_source") // ID of the source at the right of the env (the unstable one)
  lazy val rightSrcStop: Int =
    node.get[Integer]("stop_right_source") // time at which the source at the right of the env stops being a source
  lazy val learnCondition: Boolean = node.get[java.lang.Boolean]("learn")
  lazy val initialValue: Double = node.get[Double]("initial_value")
  // Other constant
  lazy val windowDifferenceSize: Int = node.get[java.lang.Integer]("window")
  lazy val trajectorySize: Int = node.get[java.lang.Integer]("trajectory")
  // Learning constants
  lazy val alpha: TimeVariable[Double] = node.get("alpha")
  lazy val beta: TimeVariable[Double] = node.get("beta")
  lazy val epsilon: TimeVariable[Double] = node.get("epsilon")
  lazy val gamma: Double = node.get[java.lang.Double]("gamma")
  // Q Learning data
  lazy val actions: NonEmptySet[Action] = node.get("actions")
  // Pickle loose the default, so we need to replace it each time the map is loaded
  lazy val q: Q[State, Action] =
    qTableStorage.loadOrElse(mid().toString, Q.zeros[State, Action]()).withDefault(initialValue)
  lazy val qLearning = QLearning.Hysteretic[List[Int], Int](actions, alpha, beta, gamma)
  // Source condition
  override def source: Boolean =
    if (mid() == leftSrc || (mid() == rightSrc && passedTime < rightSrcStop)) true else false
  // Aggregate Program data
  lazy val clock: Clock = clockTableStorage.loadOrElse(mid().toString, Clock.start)
  // Store data
  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of unsafe scala binding
  lazy val store: EndHandler[_] = {
    val storeMonitor = new EndHandler[Any](() => {
      qTableStorage.save(mid().toString, node.get[Q[List[Int], Int]]("qtable"))
      clockTableStorage.save(mid().toString, node.get[Clock]("clock"))
    })
    alchemistEnvironment.getSimulation.addOutputMonitor(storeMonitor)
    storeMonitor
  }
  // Aggregate program
  override def main(): Any = {
    val classicHopCount = hopGradient(source) // BASELINE
    val hopCountWithoutRightSource =
      hopGradient(mid() == leftSrc) // optimal gradient when RIGHT_SRC stops being a source
    val refHopCount = if (passedTime >= rightSrcStop) hopCountWithoutRightSource else classicHopCount
    // Learning definition
    val learningProblem = learningProcess(q)
      .stateDefinition(stateFromWindow)
      .rewardDefinition(output => rewardSignal(refHopCount.toInt, output))
      .actionEffectDefinition((output, action) => output + action + 1)
      .initialConditionDefinition(List.empty, Double.PositiveInfinity)
    // RL Program execution
    val roundData = mux(learnCondition && !source) {
      learningProblem.learn(qLearning, epsilon, clock)
    } {
      learningProblem.act(qLearning, clock)
    }
    val stateOfTheArt = svdGradient()(source = source, () => 1)
    val rlBasedError = refHopCount - roundData.output
    val overEstimate =
      if (rlBasedError > 0) { 1 }
      else { 0 }
    val underEstimate =
      if (rlBasedError < 0) { 1 }
      else { 0 }
    // Store alchemist info
    node.put("overestimate", overEstimate)
    node.put("underestimate", underEstimate)
    node.put("qtable", roundData.q)
    node.put("clock", roundData.clock)
    node.put("classicHopCount", classicHopCount)
    node.put("rlbasedHopCount", roundData.output)
    node.put(s"err_classicHopCount", Math.abs(refHopCount - classicHopCount))
    node.put(s"err_rlbasedHopCount", Math.abs(refHopCount - roundData.output))
    node.put(s"passed_time", passedTime)
    node.put("src", source)
    node.put("action", roundData.action)
    node.put(s"err_flexHopCount", Math.abs(refHopCount - stateOfTheArt))
    // Store update data
    store // lazy value that initialize the output monitor
  }

  private def stateFromWindow(output: Double): State = {
    val minOutput = minHood(nbr(output)).toInt
    val recent = recentValues(windowDifferenceSize, minOutput)
    val oldState = recent.headOption.getOrElse(minOutput)
    //val diff = (minOutput - oldState).sign
    val diff = minOutput - oldState
    //recentValues(trajectorySize, (diff, minOutput)).flatMap { case (a, b) => List(a, b) }.toList
    recentValues(trajectorySize, diff).toList
    //List(minOutput).map(_.toInt)
  }

  private def rewardSignal(groundTruth: Double, currentValue: Double): Double =
    if ((groundTruth.toInt - currentValue.toInt) == 0) { 0 }
    else { -1 }

}
