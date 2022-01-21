package it.unibo.scafi.casestudy.algorithm.gradient

import cats.data.NonEmptySet
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ScafiAlchemistSupport
import it.unibo.learning.{Q, QLearning}
import it.unibo.scafi.casestudy.algorithm.RLLike
import it.unibo.scafi.casestudy.algorithm.RLLike.AlgorithmHyperparameter
import it.unibo.scafi.casestudy.algorithm.hopcount.TemporalRL
import it.unibo.scafi.casestudy.algorithm.hopcount.TemporalRL.{Action, State}
import it.unibo.scafi.casestudy.{GradientLikeLearning, TemporalStateManagement}
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import scala.util.Random

trait TemporalGradientRL extends RLLike {
  self: AggregateProgram with TemporalStateManagement with GradientLikeLearning with ScafiAlchemistSupport =>
  class TemporalRLAlgorithm(
      parameter: AlgorithmHyperparameter,
      actionSet: NonEmptySet[Action],
      maxDiff: Int,
      windowDifferenceSize: Int,
      trajectorySize: Int
  )(implicit rand: Random)
      extends AlgorithmTemplate[State, Action] {
    override val name: String = "temporalRL"

    override protected def learning: QLearning.Type[State, Action] =
      QLearning.Hysteretic[State, Action](actionSet, parameter.alpha, parameter.beta, parameter.gamma)

    override protected def state(output: Double, action: Action): State = {
      val minOutput = minHood(nbr(output))
      val recent = recentValues(windowDifferenceSize, minOutput)
      val oldState = recent.headOption.getOrElse(minOutput)
      val diff = (minOutput - oldState) match {
        case diff if Math.abs(diff) > maxDiff => maxDiff * diff.sign
        case diff                             => diff
      }
      recentValues(trajectorySize, diff).toList.map(_.toInt)
    }

    override protected def actionEffect(oldOutput: Double, state: State, action: Action): Double =
      minHoodPlus(nbr(oldOutput)) + action + 1

    override protected def initialState: State = List.empty

    override protected def q: Q[State, Action] = TemporalRL.q
  }
}

object TemporalGradientRL {
  trait State
  case object Greater extends State
  case object Smaller extends State
  case object GreaterTwoTimes extends State
  case object SmallerTwoTime extends State
  type Action = Double // velocity
}
