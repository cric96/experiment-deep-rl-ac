package it.unibo.scafi.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.learning._
import it.unibo.scafi.casestudy.LearningProcess.{BuilderFinalizer, LearningContext}
import monocle.syntax.all._
import scala.util.Random

trait HopCountQLearning {
  self: AggregateProgram =>
  // TEMPLATE METHOD
  def source: Boolean

  case class HopCountState[S, A](q: Q[S, A], state: S, action: A, output: Double, clock: Clock) {
    def view: (Q[S, A], Double) = (q, output)
  }

  def learningProcess[S, A](initialQ: Q[S, A]): LearningProcess.QBuilderStep[S, A, Double] =
    LearningProcess.QBuilderStep(initialQ)

  implicit class HopCountFinalizer[S, A](ctx: LearningContext[S, A, Double]) extends BuilderFinalizer[S, A, Double] {
    override def learn(
        qLearning: QLearning[S, A],
        epsilon: TimeVariable[Double],
        initialClock: Clock
    )(implicit rnd: Random): (Q[S, A], Double) = {
      val action = Policy.greedy(ctx.q, ctx.initialCondition.state, qLearning.actions)
      val stateEvolution =
        HopCountState(ctx.q, ctx.initialCondition.state, action, ctx.initialCondition.output, initialClock)
      rep(stateEvolution) { ev =>
        // Q-Learning update
        val stateTPlus = ctx.statePolicy(stateEvolution.output)
        val reward = ctx.rewardSignal(stateEvolution.output)
        val updatedQ = qLearning.improve(
          (stateEvolution.state, stateEvolution.action, reward, stateTPlus),
          ctx.q,
          stateEvolution.clock
        )
        // Agent update
        val nextAction =
          Policy.epsilonGreedy(updatedQ, ev.state, qLearning.actions, epsilon.value(ev.clock))
        val nextOutput = hopCount(nextAction, ctx)
        ev
          .focus(_.q)
          .replace(updatedQ)
          .focus(_.clock)
          .modify(_.tick)
          .focus(_.output)
          .replace(nextOutput)
          .focus(_.action)
          .replace(nextAction)
      }.view
    }

    override def act(qLearning: QLearning[S, A])(implicit random: Random): (Q[S, A], Double) = {
      val action = Policy.greedy(ctx.q, ctx.initialCondition.state, qLearning.actions)
      val stateEvolution =
        HopCountState(ctx.q, ctx.initialCondition.state, action, ctx.initialCondition.output, Clock.start)
      rep(stateEvolution) { ev =>
        val stateTPlus = ctx.statePolicy(stateEvolution.output)
        val nextAction = Policy.greedy(ctx.q, ev.state, qLearning.actions)
        val nextOutput = hopCount(nextAction, ctx)
        ev.focus(_.output)
          .replace(nextOutput)
          .focus(_.action)
          .replace(nextAction)
          .focus(_.state)
          .replace(stateTPlus)
      }.view
    }

    private def hopCount(action: A, ctx: LearningContext[S, A, Double]): Double = {
      rep(ctx.initialCondition.output) { hopCount =>
        mux(source)(0.0)(ctx.actionEffect(minHood(nbr(hopCount)) + 1, action))
      }
    }
  }
}
