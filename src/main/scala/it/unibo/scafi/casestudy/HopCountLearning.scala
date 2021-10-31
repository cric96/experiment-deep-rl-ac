package it.unibo.scafi.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.learning._
import it.unibo.scafi.casestudy.LearningProcess.{BuilderFinalizer, LearningContext, RoundData, Trajectory}
import monocle.syntax.all._

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
      trajectory: Trajectory[S, A],
      clock: Clock
  ) {
    def view(learning: Sars.Type[S, A, T]): (RoundData[S, A, Double], Trajectory[S, A]) =
      (RoundData(learning.extractQFromTarget(target), output, action, clock), trajectory)
  }

  def learningProcess[S, A](initialQ: Q[S, A]): LearningProcess.QBuilderStep[S, A, Double] =
    LearningProcess.QBuilderStep(initialQ)

  implicit class HopCountFinalizer[S, A](ctx: LearningContext[S, A, Double]) extends BuilderFinalizer[S, A, Double] {
    override def learn[T](
        learning: Sars.Type[S, A, T],
        epsilon: TimeVariable[Double],
        clock: Clock
    )(implicit rnd: Random): (RoundData[S, A, Double], Trajectory[S, A]) = {
      val action = Policy.greedy(learning.actions)(ctx.initialCondition.state, ctx.q, clock)
      val epsilonGreedy = Policy.epsilonGreedy[S, A](learning.actions, epsilon)
      val stateEvolution =
        HopCountState[S, A, learning.Aux](
          learning.initTargetFromQ(ctx.q),
          ctx.initialCondition.state,
          action,
          ctx.initialCondition.output,
          Vector.empty,
          clock
        )
      rep(stateEvolution) { ev =>
        val nextOutput = hopCount(ev.action, ctx)
        val stateTPlus = ctx.statePolicy(nextOutput)
        val reward = ctx.rewardSignal(nextOutput)
        // Agent update
        val updateTargetLearning = learning.improve(
          (ev.state, ev.action, reward, stateTPlus),
          ev.target,
          ev.clock
        )
        val nextAction = epsilonGreedy(stateTPlus, learning.extractQFromTarget(ev.target), clock)
        ev
          .focus(_.target)
          .replace(updateTargetLearning)
          .focus(_.clock)
          .modify(_.tick)
          .focus(_.output)
          .replace(nextOutput)
          .focus(_.action)
          .replace(nextAction)
          .focus(_.state)
          .replace(stateTPlus)
          .focus(_.trajectory)
          .modify(trajectory => trajectory :+ (ev.state, action, reward))
      }.view(learning)
    }

    override def act[T](learning: Sars.Type[S, A, T], clock: Clock)(implicit
        rand: Random
    ): (RoundData[S, A, Double], Trajectory[S, A]) = {
      val greedy = Policy.greedy[S, A](learning.actions)
      val action = greedy(ctx.initialCondition.state, ctx.q, clock)
      val stateEvolution =
        HopCountState[S, A, T](
          learning.initTargetFromQ(ctx.q),
          ctx.initialCondition.state,
          action,
          ctx.initialCondition.output,
          Vector.empty,
          clock = clock
        )
      rep(stateEvolution) { ev =>
        val nextOutput = hopCount(ev.action, ctx)
        val stateTPlus = ctx.statePolicy(nextOutput)
        val nextAction =
          greedy(stateTPlus, learning.extractQFromTarget(ev.target), clock)
        ev
          .focus(_.clock)
          .modify(_.tick)
          .focus(_.output)
          .replace(nextOutput)
          .focus(_.action)
          .replace(nextAction)
          .focus(_.state)
          .replace(stateTPlus)
      }.view(learning)
    }

    private def hopCount(action: A, ctx: LearningContext[S, A, Double]): Double = {
      rep(ctx.initialCondition.output) { hopCount =>
        mux(source)(0.0)(ctx.actionEffect(minHoodPlus(nbr(hopCount)), action))
      }
    }
  }
}
