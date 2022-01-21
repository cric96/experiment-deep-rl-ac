package it.unibo.scafi.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.learning._
import it.unibo.scafi.casestudy.LearningProcess.{BuilderFinalizer, LearningContext, RoundData, Trajectory}
import monocle.syntax.all._

import scala.language.reflectiveCalls
import scala.util.Random

trait GradientLikeLearning {
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
  @SuppressWarnings(
    Array("org.wartremover.warts.DefaultArguments", "org.wartremover.warts.TraversableOps")
  )
  implicit class HopCountFinalizer[S, A](ctx: LearningContext[S, A, Double]) extends BuilderFinalizer[S, A, Double] {
    override def step[T](
        learning: Sars.Type[S, A, T],
        epsilon: Double,
        learn: Boolean = false
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
        val nextOutput = hopCount(ev.state, ev.action, ctx)
        val stateTPlus = ctx.statePolicy(nextOutput, ev.action)
        val reward = ctx.rewardSignal(nextOutput)
        // Agent update
        val updateTargetLearning = branch(learn) {
          learning.improve(
            (ev.state, ev.action, reward, stateTPlus),
            ev.target
          )
        } {
          ev.target
        }
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

    private def hopCount(state: S, action: A, ctx: LearningContext[S, A, Double]): Double = {
      rep(ctx.initialCondition.output) { hopCount =>
        mux(source)(0.0)(ctx.actionEffect(hopCount, state, action))
      }
    }
  }
}
