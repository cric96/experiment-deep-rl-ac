package it.unibo.scafi.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.learning._
import it.unibo.scafi.casestudy.LearningProcess.{RoundData, BuilderFinalizer, LearningContext}
import monocle.syntax.all._

import scala.util.Random

trait HopCountQLearning {
  self: AggregateProgram =>
  // TEMPLATE METHOD
  def source: Boolean

  case class HopCountState[S, A](q: Q[S, A], state: S, action: A, output: Double, clock: Clock) {
    def view: RoundData[S, A, Double] = RoundData(q, output, action, clock)
  }

  def learningProcess[S, A](initialQ: Q[S, A]): LearningProcess.QBuilderStep[S, A, Double] =
    LearningProcess.QBuilderStep(initialQ)

  implicit class HopCountFinalizer[S, A](ctx: LearningContext[S, A, Double]) extends BuilderFinalizer[S, A, Double] {
    override def learn(
        qLearning: QLearning.Type[S, A],
        epsilon: TimeVariable[Double],
        clock: Clock
    )(implicit rnd: Random): RoundData[S, A, Double] = {
      val action = Policy.greedy(ctx.q, ctx.initialCondition.state, qLearning.actions)
      val stateEvolution =
        HopCountState(ctx.q, ctx.initialCondition.state, action, ctx.initialCondition.output, clock)
      rep(stateEvolution) { ev =>
        val nextOutput = hopCount(ev.action, ctx)
        val stateTPlus = ctx.statePolicy(nextOutput)
        val reward = ctx.rewardSignal(nextOutput)
        val updatedQ = qLearning.improve(
          (ev.state, ev.action, reward, stateTPlus),
          ev.q,
          ev.clock
        )
        // Agent update
        val nextAction =
          Policy.epsilonGreedy(updatedQ, stateTPlus, qLearning.actions, epsilon.value(ev.clock))
        ev
          .focus(_.q)
          .replace(updatedQ)
          .focus(_.clock)
          .modify(_.tick)
          .focus(_.output)
          .replace(nextOutput)
          .focus(_.action)
          .replace(nextAction)
          .focus(_.state)
          .replace(stateTPlus)
      }.view
    }

    override def act(qLearning: QLearning.Type[S, A], clock: Clock)(implicit
        random: Random
    ): RoundData[S, A, Double] = {
      val action = Policy.greedy(ctx.q, ctx.initialCondition.state, qLearning.actions)
      val stateEvolution =
        HopCountState(ctx.q, ctx.initialCondition.state, action, ctx.initialCondition.output, clock)
      rep(stateEvolution) { ev =>
        val nextOutput = hopCount(ev.action, ctx)
        val stateTPlus = ctx.statePolicy(nextOutput)
        val nextAction =
          Policy.greedy(ev.q, stateTPlus, qLearning.actions)
        ev
          .focus(_.clock)
          .modify(_.tick)
          .focus(_.output)
          .replace(nextOutput)
          .focus(_.action)
          .replace(nextAction)
          .focus(_.state)
          .replace(stateTPlus)
      }.view
    }

    private def hopCount(action: A, ctx: LearningContext[S, A, Double]): Double = {
      rep(ctx.initialCondition.output) { hopCount =>
        mux(source)(0.0)(ctx.actionEffect(minHoodPlus(nbr(hopCount)), action))
      }
    }
  }
}
