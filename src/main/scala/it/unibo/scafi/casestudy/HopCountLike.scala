package it.unibo.scafi.casestudy

import cats.data.NonEmptySet
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.alchemist.tiggers.EndHandler
import it.unibo.learning.{Episode, Q, TimeVariable}
import it.unibo.scafi.casestudy.LearningProcess.RoundData
import it.unibo.storage.LocalStorage

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
  // Implicit context variable
  implicit lazy val rand: Random = randomGen
  // Storage
  lazy val episode: Episode = Episode(node.get[java.lang.Double]("episode").toInt)
  // Variable loaded by alchemist configuration.
  lazy val learnCondition: Boolean = node.get[java.lang.Boolean]("learn")
  lazy val initialValue: Double = node.get[Double]("initial_value")
  // Learning constants
  lazy val alpha: TimeVariable[Double] = node.get("alpha")
  lazy val beta: TimeVariable[Double] = node.get("beta")
  lazy val epsilon: TimeVariable[Double] = node.get("epsilon")
  lazy val gamma: Double = node.get[java.lang.Double]("gamma")
  // Store data
  def endHandler: EndHandler[_]

  def aggregateProgram(): Unit

  final override def main(): Any =
    (aggregateProgram(), endHandler)

  protected def passedTime(): Double = alchemistTimestamp.toDouble
}
