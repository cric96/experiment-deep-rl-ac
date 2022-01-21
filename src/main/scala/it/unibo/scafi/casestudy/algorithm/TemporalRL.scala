package it.unibo.scafi.casestudy.algorithm
import cats.data.NonEmptySet
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.learning.Q.MutableQ
import it.unibo.learning.{Q, QLearning}
import it.unibo.scafi.casestudy.algorithm.RLLike.AlgorithmHyperparameter
import it.unibo.scafi.casestudy.algorithm.TemporalRL._
import it.unibo.scafi.casestudy.{GradientLikeLearning, TemporalStateManagement}

import scala.util.Random
trait TemporalRL extends RLLike {
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

object TemporalRL {
  type State = List[Int]
  type Action = Int
  val q = MutableQ[State, Action](Map.empty).withDefault(0.0)
}
