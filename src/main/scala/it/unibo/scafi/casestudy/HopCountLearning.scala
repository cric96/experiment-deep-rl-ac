package it.unibo.scafi.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.learning.ReinforcementLearning.Ops
import it.unibo.learning._
import it.unibo.scafi.casestudy.LearningProcess.{BuilderFinalizer, LearningContext, RoundData, Trajectory}
import monocle.syntax.all._

import scala.language.reflectiveCalls
import scala.util.Random

trait HopCountLearning {
  self: AggregateProgram =>
  // TEMPLATE METHOD
  def source: Boolean

  case class HopCountState[S, A, T](
      target: T,
      state: S,
      action: A,
      output: Double,
      trajectory: Trajectory[S, A]
  ) {
    def view(learning: ReinforcementLearning.Ops[S, A, T]): (RoundData[S, A, Double], Trajectory[S, A]) =
      (RoundData(learning.extractQFromTarget(target), output, action), trajectory)
  }

  def learningProcess[S, A](initialQ: Q[S, A]): LearningProcess.QBuilderStep[S, A, Double] =
    LearningProcess.QBuilderStep(initialQ)

  implicit class HopCountFinalizer[S, A](ctx: LearningContext[S, A, Double]) extends BuilderFinalizer[S, A, Double] {
    override def learn[T](
        learning: Sars.Type[S, A, T],
        epsilon: Double
    )(implicit rnd: Random): (RoundData[S, A, Double], Trajectory[S, A]) = {
      val action = Policy.greedy(learning.actions)(ctx.initialCondition.state, ctx.q)
      val epsilonGreedy = Policy.epsilonGreedy[S, A](learning.actions, epsilon)
      val stateEvolution =
        HopCountState[S, A, learning.ops.Aux](
          learning.ops.initTargetFromQ(ctx.q),
          ctx.initialCondition.state,
          action,
          ctx.initialCondition.output,
          List.empty
        )
      rep(stateEvolution) { ev =>
        val nextOutput = hopCount(ev.action, ctx)
        val stateTPlus = ctx.statePolicy(nextOutput)
        val reward = ctx.rewardSignal(nextOutput)
        // Agent update
        val updateTargetLearning = learning.improve(
          (ev.state, ev.action, reward, stateTPlus),
          ev.target
        )
        val nextAction = epsilonGreedy(stateTPlus, learning.ops.extractQFromTarget(ev.target))
        ev
          .focus(_.trajectory)
          .modify(trajectory => (ev.state, ev.action, reward) :: trajectory.toList)
          .focus(_.target)
          .replace(updateTargetLearning)
          .focus(_.output)
          .replace(nextOutput)
          .focus(_.action)
          .replace(nextAction)
          .focus(_.state)
          .replace(stateTPlus)
      }.view(learning.ops)
    }

    override def actGreedy[T](learning: Sars.Type[S, A, T])(implicit
        rand: Random
    ): (RoundData[S, A, Double], Trajectory[S, A]) = actWith(learning.ops, Policy.greedy(learning.actions))

    override def actWith[T](
        learningInstance: Ops[S, A, T],
        policy: Policy.QBased[S, A]
    )(implicit
        rand: Random
    ): (RoundData[S, A, Double], Trajectory[S, A]) = {
      val action = policy(ctx.initialCondition.state, ctx.q)
      val stateEvolution =
        HopCountState[S, A, T](
          learningInstance.initTargetFromQ(ctx.q),
          ctx.initialCondition.state,
          action,
          ctx.initialCondition.output,
          List.empty
        )
      rep(stateEvolution) { ev =>
        val nextOutput = hopCount(ev.action, ctx)
        val reward = ctx.rewardSignal(nextOutput)
        val stateTPlus = ctx.statePolicy(nextOutput)
        val nextAction =
          policy(stateTPlus, learningInstance.extractQFromTarget(ev.target))
        ev
          .focus(_.trajectory)
          .modify(trajectory => (ev.state, ev.action, reward) :: trajectory.toList)
          .focus(_.output)
          .replace(nextOutput)
          .focus(_.action)
          .replace(nextAction)
          .focus(_.state)
          .replace(stateTPlus)
      }.view(learningInstance)
    }

    private def hopCount(action: A, ctx: LearningContext[S, A, Double]): Double = {
      rep(ctx.initialCondition.output) { hopCount =>
        mux(source)(0.0)(ctx.actionEffect(minHoodPlus(nbr(hopCount)), action))
      }
    }
  }
}
