package it.unibo.scafi.casestudy.algorithm.gradient

import cats.data.NonEmptySet
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.{ScafiAlchemistSupport, _}
import it.unibo.learning.Q.MutableQ
import it.unibo.learning.{Q, QLearning}
import it.unibo.scafi.casestudy.algorithm.RLLike
import it.unibo.scafi.casestudy.algorithm.RLLike.AlgorithmHyperparameter
import it.unibo.scafi.casestudy.algorithm.gradient.TemporalGradientRL._
import it.unibo.scafi.casestudy.{GradientLikeLearning, TemporalStateManagement}

import scala.util.Random

trait TemporalGradientRL extends RLLike {
  self: AggregateProgram
    with TemporalStateManagement
    with GradientLikeLearning
    with ScafiAlchemistSupport
    with StandardSensors =>
  // move to another part?
  implicit class DoubleWithAlmostEquals(val d: Double) {
    def default = 0.00001
    def ~=(d2: Double) = (d - d2).abs < default
  }
  class TemporalRLAlgorithm(
      parameter: AlgorithmHyperparameter,
      actionSet: NonEmptySet[Action],
      radius: Double,
      maxDiff: Int,
      windowDifferenceSize: Int,
      trajectorySize: Int
  )(implicit rand: Random)
      extends AlgorithmTemplate[History, Action] {
    override val name: String = "temporalRL"

    override protected def learning: QLearning.Type[History, Action] =
      QLearning.Hysteretic[History, Action](actionSet, parameter.alpha, parameter.beta, parameter.gamma)

    override protected def state(output: Double, action: Action): History = {
      def evalState(out: Double): GradientDifference = {
        val diff = output - out
        if (diff ~= 0) {
          Same
        } else if (diff > 0 && diff < 2 * radius) {
          Greater
        } else if (diff > 0 && diff > 2 * radius) {
          GreaterTwoTimes
        } else if (diff < 0 && diff > -(2 * radius)) {
          Smaller
        } else {
          SmallerTwoTime
        }
      }
      val minOutput = evalState(minHood(nbr(output)))
      val maxOutput = evalState(maxHood(nbr(output)))
      val recent = recentValues(windowDifferenceSize, State(maxOutput, minOutput))
      History(recent.toList)
    }

    override protected def actionEffect(oldOutput: Double, state: History, action: Action): Double =
      mux(source)(0.0) {
        val result = minHoodPlus(nbr(oldOutput) + nbrRange())
        action match {
          case TemporalGradientRL.ConsiderNeighbourhood => result
          case TemporalGradientRL.Ignore(upVelocity) => oldOutput + upVelocity * deltaTime().toMillis.toDouble / 1000.0
        }
      }

    override protected def initialState: History = History(Seq.empty)

    override protected def q: Q[History, Action] = TemporalGradientRL.q
  }
}

object TemporalGradientRL {
  trait GradientDifference
  case object Same extends GradientDifference
  case object Greater extends GradientDifference
  case object Smaller extends GradientDifference
  case object GreaterTwoTimes extends GradientDifference
  case object SmallerTwoTime extends GradientDifference
  case class State(maxDifference: GradientDifference, minDifference: GradientDifference)
  case class History(states: Seq[State])

  trait Action
  implicit def ordering: Ordering[Action] = (x: Action, y: Action) =>
    (x, y) match {
      case (ConsiderNeighbourhood, Ignore(_))             => 1
      case (Ignore(_), ConsiderNeighbourhood)             => -1
      case (ConsiderNeighbourhood, ConsiderNeighbourhood) => 0
      case (Ignore(l), Ignore(r))                         => Ordering.Double.IeeeOrdering.compare(l, r)
    }
  case class Ignore(upVelocity: Double) extends Action
  case object ConsiderNeighbourhood extends Action

  val q = new MutableQ[History, Action](Map.empty).withDefault(0.0)
}
