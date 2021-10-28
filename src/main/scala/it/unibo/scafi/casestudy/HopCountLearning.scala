package it.unibo.scafi.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.learning._
import it.unibo.scafi.casestudy.LearningProcess.{BuilderFinalizer, LearningContext, RoundData}
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
      clock: Clock
  ) {
    def view(learning: Sars.Type[S, A, T]): RoundData[S, A, Double] =
      RoundData(learning.extractQFromTarget(target), output, action, clock)
  }

  def learningProcess[S, A](initialQ: Q[S, A]): LearningProcess.QBuilderStep[S, A, Double] =
    LearningProcess.QBuilderStep(initialQ)

  implicit class HopCountFinalizer[S, A](ctx: LearningContext[S, A, Double]) extends BuilderFinalizer[S, A, Double] {
    override def learn[T](
        learning: Sars.Type[S, A, T],
        epsilon: TimeVariable[Double],
        clock: Clock
    )(implicit rnd: Random): RoundData[S, A, Double] = {
      val action = Policy.greedy(ctx.q, ctx.initialCondition.state, learning.actions)
      val stateEvolution =
        HopCountState[S, A, learning.Aux](
          learning.initTargetFromQ(ctx.q),
          ctx.initialCondition.state,
          action,
          ctx.initialCondition.output,
          clock
        )
      rep(stateEvolution) { ev =>
        val nextOutput = hopCount(ev.action, ctx)
        val stateTPlus = ctx.statePolicy(nextOutput)
        val reward = ctx.rewardSignal(nextOutput)
        val updateTargetLearning = learning.improve(
          (ev.state, ev.action, reward, stateTPlus),
          ev.target,
          ev.clock
        )
        // Agent update
        val nextAction =
          Policy.epsilonGreedy(
            learning.extractQFromTarget(ev.target),
            stateTPlus,
            learning.actions,
            epsilon.value(ev.clock)
          )
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
      }.view(learning)
    }

    override def act[T](learning: Sars.Type[S, A, T], clock: Clock)(implicit
        rand: Random
    ): RoundData[S, A, Double] = {
      val action = Policy.greedy(ctx.q, ctx.initialCondition.state, learning.actions)
      val stateEvolution =
        HopCountState[S, A, T](
          learning.initTargetFromQ(ctx.q),
          ctx.initialCondition.state,
          action,
          ctx.initialCondition.output,
          clock
        )
      rep(stateEvolution) { ev =>
        val nextOutput = hopCount(ev.action, ctx)
        val stateTPlus = ctx.statePolicy(nextOutput)
        val nextAction =
          Policy.greedy(learning.extractQFromTarget(ev.target), stateTPlus, learning.actions)
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
