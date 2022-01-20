package it.unibo.scafi.casestudy.algorithm

import it.unibo.alchemist.model.implementations.nodes.NodeManager
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.learning.{Q, QLearning}
import it.unibo.scafi.casestudy.{HopCountLearning, LearningProcess}
import it.unibo.scafi.casestudy.LearningProcess.{LearningContext, RoundData, Trajectory}

import scala.util.Random

trait RLLike {
  self: AggregateProgram with ScafiAlchemistSupport with HopCountLearning =>
  /* UTILITIES */
  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps")) // because of unsafe scala binding
  protected def globalSignal[S, A](
      trajectory: Trajectory[S, A],
      lastCorrectValue: Double,
      lastOutput: Double,
      globalReward: Double
  ): Double = {
    val (lastState, _, _) = trajectory.head
    val (lastMinusOne, action, _) = trajectory.tail.head
    if (lastCorrectValue.toInt != lastOutput.toInt) { globalReward }
    else { 0 }
  }
  /* ALGORITHM TEMPLATE*/
  abstract class AlgorithmTemplate[S, A](implicit rand: Random) {
    type Output = (RoundData[S, A, Double], Trajectory[S, A])
    def name: String
    def learningProblem: LearningContext[S, A, Double] = LearningProcess
      .QBuilderStep(q)
      .stateDefinition(state)
      .rewardDefinition(rewardSignal)
      .actionEffectDefinition(actionEffect)
      .initialConditionDefinition(initialState, initialOutput)
    final def output(shouldLearn: Boolean, epsilon: Double): Output =
      learningProblem.step(learning, epsilon, shouldLearn)
    def episodeEnd(nodes: Iterable[NodeManager]): Unit = {}
    def peekReference: Double = node.get("reference")

    protected def learning: QLearning.Type[S, A]
    protected def state(output: Double, action: A): S
    protected def actionEffect(oldOutput: Double, state: S, action: A): Double
    protected def rewardSignal(output: Double): Double = {
      if ((peekReference.toInt - output.toInt) == 0) {
        0
      } else { -1 }
    }
    protected def initialState: S
    protected def initialOutput: Double = Double.PositiveInfinity
    protected def q: Q[S, A]
  }
}

object RLLike {
  /* DATA CLASS */
  case class AlgorithmHyperparameter(alpha: Double, beta: Double, gamma: Double)
}
