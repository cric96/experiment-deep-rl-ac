package it.unibo.scafi.casestudy.algorithm.hopcount

import cats.data.NonEmptySet
import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.{ScafiAlchemistSupport, _}
import it.unibo.learning.Q.MutableQ
import it.unibo.learning.{Q, QLearning}
import it.unibo.scafi.casestudy.algorithm.RLLike
import it.unibo.scafi.casestudy.algorithm.RLLike.AlgorithmHyperparameter
import it.unibo.scafi.casestudy.algorithm.hopcount.GlobalViewRL.{Action, State}
import it.unibo.scafi.casestudy.{GradientLikeLearning, TemporalStateManagement}

import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.Random
trait GlobalViewRL extends RLLike {
  self: AggregateProgram with TemporalStateManagement with GradientLikeLearning with ScafiAlchemistSupport =>

  class GlobalViewAlgorithm(parameters: AlgorithmHyperparameter, actions: NonEmptySet[Action])(implicit
      val rand: Random
  ) extends AlgorithmTemplate[State, Action] {
    override val name: String = "globalViewRL"

    override protected def learning: QLearning.Type[State, Action] = QLearning.Hysteretic[State, Action](
      actions,
      parameters.alpha,
      parameters.beta,
      parameters.gamma
    )
    @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of alchemist implementation
    override protected def state(output: Double, action: Action): State = {
      val nodes = alchemistEnvironment.getNodes.iterator().asScala.toList.map(node => new SimpleNodeManager(node))
      val prevOutput = nodes.collect {
        case node if node.has(name) => node.get[Int](name)
        case _                      => Double.PositiveInfinity.toInt
      }
      prevOutput
    }

    override protected def actionEffect(oldOutput: Double, state: State, action: Action): Double =
      minHoodPlus(nbr(oldOutput)) + action + 1

    override protected def initialState: State = List.empty

    override protected def q: Q[State, Action] = GlobalViewRL.q
  }
}

object GlobalViewRL {
  type State = List[Int]
  type Action = Int
  val q: Q[State, Action] = MutableQ[State, Action](Map.empty).withDefault(0.0)
}
