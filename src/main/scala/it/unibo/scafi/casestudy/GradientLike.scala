package it.unibo.scafi.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.alchemist.tiggers.EndHandler

/** Common variable/constants/behaviour that have hop count learning problem */
trait GradientLike
    extends AggregateProgram
    with StandardSensors
    with ScafiAlchemistSupport
    with BlockT
    with BlockG
    with FieldUtils
    with TemporalStateManagement
    with FixedGradients {
  // Variable loaded by alchemist configuration.
  lazy val learnCondition: Boolean = node.get[java.lang.Boolean]("learn")
  // Store data
  def endHandler: EndHandler[_]

  def aggregateProgram(): Unit

  final override def main(): Any =
    (aggregateProgram(), endHandler)

  protected def passedTime(): Double = alchemistTimestamp.toDouble
}
