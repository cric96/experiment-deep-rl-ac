package it.unibo.scafi.casestudy.algorithm

import it.unibo.alchemist.model.implementations.nodes.NodeManager
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.learning.{Q, QLearning}
import it.unibo.scafi.casestudy.LearningProcess.{LearningContext, RoundData, Trajectory}
import it.unibo.scafi.casestudy.{GradientLikeLearning, LearningProcess}

import scala.util.Random

trait RLLike {
  self: AggregateProgram with ScafiAlchemistSupport with GradientLikeLearning =>
  /* UTILITIES */
  // An example of a global signal. Not used in experiments but is could be useful for future experiments
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

  /** This class defines a general structure of a learning algorithm applied to aggregate computing at the algorithm
    * level. In this case, the node state in strongly linked with the local computational field.
    *
    * To make the learning possible, a user has to define:
    *
    * state policy: given the output produced in the current step and the action that the node is going to use, this
    * method produce the state used by the RL algorithm.
    *
    * actionEffect: define the impact of the action of this node. It will output the current local field using the state
    * and the action taken
    *
    * rewardSignal: describe the positive/negative impact of the action taken according the current node environment
    * perceived (typically local, but global information could be used).
    *
    * learning: the learning instance used to improve the q table
    *
    * q: the q table that will be improve during the learning. In this case it is mutable and define at the global level
    * @param reference
    *   the name of the molecule where the reference output is stored
    * @param rand
    *   contextual information (given in scala 3).
    * @tparam S
    *   state type space
    * @tparam A
    *   action type space
    */
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments")) // because it is applied at the class level
  abstract class AlgorithmTemplate[S, A](reference: String = "reference")(implicit rand: Random) {
    type Output = (RoundData[S, A, Double], Trajectory[S, A])
    def name: String
    // TEMPLATE METHOD
    def learningProblem: LearningContext[S, A, Double] = LearningProcess
      .QBuilderStep(q)
      .stateDefinition(state)
      .rewardDefinition(rewardSignal)
      .actionEffectDefinition(actionEffect)
      .initialConditionDefinition(initialState, initialOutput)

    def output(shouldLearn: Boolean, epsilon: Double): Output =
      learningProblem.step(learning, epsilon, shouldLearn)
    def episodeEnd(nodes: Iterable[NodeManager]): Unit = {}

    def peekReference: Double = node.get(reference)

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
