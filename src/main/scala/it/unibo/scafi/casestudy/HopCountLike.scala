package it.unibo.scafi.casestudy

import cats.data.NonEmptySet
import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.alchemist.tiggers.EndHandler
import it.unibo.learning.{Clock, Q, QLearning, TimeVariable}
import it.unibo.scafi.casestudy.LearningProcess.RoundData
import it.unibo.storage.LocalStorage

import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.Random

/** Common variable/constants/behaviour that have hop count learning problem */
trait HopCountLike
    extends AggregateProgram
    with HopCountLearning
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
  lazy val episode: Int = node.get[java.lang.Double]("episode").toInt
  lazy val clockTableStorage = new LocalStorage[String](node.get[java.lang.String]("clock_folder"))
  def passedTime: Double = alchemistTimestamp.toDouble
  // Variable loaded by alchemist configuration.
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
  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of unsafe scala binding
  def qId: String
  // Pickle loose the default, so we need to replace it each time the map is loaded
  lazy val q: Q[State, Action] =
    qTableStorage.loadOrElse(qId, Q.zeros[State, Action]()).withDefault(initialValue)
  lazy val learningAlgorithm = QLearning.Hysteretic[List[Int], Int](actions, alpha, beta, gamma)
  // Aggregate Program data
  lazy val clock: Clock = clockTableStorage.loadOrElse(mid().toString, Clock.start)
  // Store data
  def endHandler: EndHandler[_]

  def aggregateProgram(): RoundData[State, Action, Double]

  final override def main(): Any =
    (aggregateProgram(), endHandler)

  protected def stateFromWindow(output: Double): State = {
    val minOutput = minHood(nbr(output))
    val recent = recentValues(windowDifferenceSize, minOutput)
    val oldState = recent.headOption.getOrElse(minOutput)
    //val diff = (minOutput - oldState).sign
    val diff = minOutput - oldState
    //recentValues(trajectorySize, (diff, minOutput)).flatMap { case (a, b) => List(a, b) }.toList
    recentValues(trajectorySize, diff).toList.map(_.toInt)
    //List(minOutput).map(_.toInt)
  }

  protected def rewardSignal(groundTruth: Double, currentValue: Double): Double =
    if ((groundTruth.toInt - currentValue.toInt) == 0) { 0 }
    else { -1 }
}
