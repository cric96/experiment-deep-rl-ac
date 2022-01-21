package it.unibo.scafi.casestudy.algorithm.hopcount

import cats.data.NonEmptySet
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.{ScafiAlchemistSupport, _}
import it.unibo.learning.Q.MutableQ
import it.unibo.learning.{Q, QLearning}
import it.unibo.scafi.casestudy.algorithm.RLLike
import it.unibo.scafi.casestudy.algorithm.RLLike.AlgorithmHyperparameter
import it.unibo.scafi.casestudy.algorithm.hopcount.CrfHopCountLikeRL._
import it.unibo.scafi.casestudy.{GradientLikeLearning, TemporalStateManagement}

import scala.collection.immutable.SortedSet
import scala.util.Random
class CrfHopCountLikeRL extends RLLike {
  self: AggregateProgram
    with TemporalStateManagement
    with GradientLikeLearning
    with ScafiAlchemistSupport
    with FieldUtils =>

  /* UTILITY AGGREGATE FUNCTIONS */
  def actionEffectTemplate(oldOutput: Double, action: Action): Double = if (action.ignoreLeft && action.ignoreRight) {
    oldOutput + action.upVelocity
  } else {
    val data = excludingSelf.reifyField(nbr(oldOutput))
    val left = data.find(data => data._1 < mid() && !action.ignoreLeft).map(_._2)
    val right = data.find(data => data._1 > mid() && !action.ignoreRight).map(_._2)
    List(left, right).collect { case Some(data) => data }.minOption.map(_ + 1).getOrElse(oldOutput)
  }

  /* ALGORITHMS */
  class CrfLikeAlgorithm(parameters: AlgorithmHyperparameter, maxDiff: Int)(implicit
      rand: Random
  ) extends AlgorithmTemplate[State, Action] {
    override def name: String = "crfLikeRL"

    override protected def learning: QLearning.Type[State, Action] =
      QLearning.Hysteretic(
        CrfHopCountLikeRL.actionSpace(velocities),
        parameters.alpha,
        parameters.beta,
        parameters.gamma
      )

    override protected def state(output: Double, action: Action): State = {
      val other = excludingSelf.reifyField(nbr(output))
      val differences = other.map { case (k, v) => k -> (output - v) }
      def align(option: Option[(ID, Double)]): Option[Int] = option
        .map(_._2)
        .map(diff =>
          if (diff.abs > maxDiff) { maxDiff * diff.sign }
          else { diff }
        )
        .map(_.toInt)
      val left = align(differences.find(_._1 < mid()))
      val right = align(differences.find(_._1 > mid()))
      State(left, right)
    }

    override protected def actionEffect(oldOutput: Double, state: State, action: Action): Double =
      actionEffectTemplate(oldOutput, action)

    override protected def initialState: State = State(None, None)

    override protected def q: Q[State, Action] = crfGlobalQ

    //protected def localSignal(currentValue: Double): Double = {
    //  val windowSize = 3
    //  if (isStable(currentValue, windowSize)) { 0 }
    //  else { -1 }
    //}
  }

  class CrfLikeWithActionAlgorithm(parameters: AlgorithmHyperparameter, maxDiff: Int)(implicit
      rand: Random
  ) extends AlgorithmTemplate[StateWithAction, Action] {
    override val name: String = "crfLikeWithActionRL"

    override protected def learning: QLearning.Type[StateWithAction, Action] =
      QLearning.Hysteretic(
        CrfHopCountLikeRL.actionSpace(velocities),
        parameters.alpha,
        parameters.beta,
        parameters.gamma
      )
    @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of unsafe scala binding
    override protected def state(output: Double, action: Action): StateWithAction = {
      val other = excludingSelf.reifyField((nbr(output), nbr(action)))
      def align(option: Option[(ID, (Double, _))]): Option[Int] = option
        .map { case (k, (out, _)) => out }
        .map(_ - output)
        .map(diff =>
          if (diff.abs > maxDiff) { maxDiff * diff.sign }
          else { diff }
        )
        .map(_.toInt)
      val left = other.find(_._1 < mid())
      val right = other.find(_._1 > mid())
      val leftOutput = align(left)
      val rightOutput = align(right)
      val leftAction = left.map(_._2._2).map(_.upVelocity)
      val rightAction = right.map(_._2._2).map(_.upVelocity)
      StateWithAction(leftOutput, leftAction, rightOutput, rightAction)
    }

    override protected def actionEffect(oldOutput: Double, state: StateWithAction, action: Action): Double =
      actionEffectTemplate(oldOutput, action)

    override protected def initialState: StateWithAction = StateWithAction(None, None, None, None)

    override protected def q: Q[StateWithAction, Action] = crfGlobalWithAction
  }
}

object CrfHopCountLikeRL {
  case class State(left: Option[Int], right: Option[Int])
  case class StateWithAction(left: Option[Int], leftAction: Option[Int], right: Option[Int], rightAction: Option[Int])
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments")) // because of unsafe scala binding
  case class Action(ignoreLeft: Boolean = false, ignoreRight: Boolean = false, upVelocity: Int = 0)

  val velocities = List(2, 4, 6) // to move in another place
  implicit def ordering: Ordering[Action] = Ordering.by(a => (a.ignoreLeft, a.ignoreRight, a.upVelocity))
  def actionSpace(velocities: List[Int]): NonEmptySet[Action] = {
    val booleanValues = List(false, true)
    val elements = velocities
    val actionSpace = for {
      left <- booleanValues
      right <- booleanValues
      up <- elements
    } yield Action(left, right, up)

    @SuppressWarnings(Array("org.wartremover.warts.All")) // because fast check
    val withoutUselessAction =
      actionSpace.filter(action => (action.ignoreRight || action.ignoreRight) || action.upVelocity == velocities.head)
    NonEmptySet.fromSetUnsafe(SortedSet(withoutUselessAction: _*))
  }

  val crfGlobalQ: Q[State, Action] = MutableQ[State, Action](Map.empty).withDefault(0.0)
  val crfGlobalWithAction: Q[StateWithAction, Action] = MutableQ[StateWithAction, Action](Map.empty).withDefault(0.0)
}
