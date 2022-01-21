package it.unibo.scafi.casestudy.algorithm.hopcount

import cats.data.NonEmptySet
import it.unibo.alchemist.model.implementations.nodes.NodeManager
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.learning.Q.MutableQ
import it.unibo.learning.{Q, QLearning}
import it.unibo.scafi.casestudy.LearningProcess.{RoundData, Trajectory}
import it.unibo.scafi.casestudy.algorithm.RLLike
import it.unibo.scafi.casestudy.algorithm.hopcount.TemporalDeepRL._
import it.unibo.scafi.casestudy.{GradientLikeLearning, LearningProcess, SwapSourceLike}
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.{PyQuote, SeqConverters}
import monocle.syntax.all._

import scala.util.Random

/** A test of a deep reinforcement learning library in aggregate computing learning problem. In particular, I had chosen
  * d3rlpy: https://github.com/takuseno/d3rlpy Scalapy helps to used that library from JVM. Currently seems that some
  * memory leak problem persist.
  */
trait TemporalDeepRL extends RLLike {
  self: AggregateProgram with ScafiAlchemistSupport with GradientLikeLearning with FieldUtils with SwapSourceLike =>
  val upperBound = 100
  class DeepRLAlgorithm(maxDiff: Int)(implicit rand: Random) extends AlgorithmTemplate[List[Int], Int] {
    val localTrajectory: Trajectory[List[Int], Int] = Seq.empty
    override def name: String = "deepRL"

    override protected def learning: QLearning.Type[List[Int], Int] =
      new QLearning.Type[List[Int], Int] { // foo implementation, to remove
        override def actions: NonEmptySet[Int] = NonEmptySet.of(1)
        override def improve(trajectory: (List[Int], Int, Double, List[Int]), target: Q[List[Int], Int])(implicit
            rand: Random
        ): Q[List[Int], Int] = q
      }

    override protected def state(output: Double, action: Int): List[Int] = {
      val other = excludingSelf.reifyField {
        val diff = nbr(output) - output
        val correction = if (diff.abs > maxDiff) { maxDiff * diff.sign }
        else { diff }
        List(correction.toInt, nbr(action))
      }
      (other.flatMap(_._2).toList ++ initialState).take(2)
    }

    override def output(
        shouldLearn: Boolean,
        epsilon: Double
    ): (LearningProcess.RoundData[List[Int], Int, Double], Trajectory[List[Int], Int]) = {
      val data = rep((HopCountState(q, initialState, 0, upperBound, Seq.empty))) { case ctx =>
        val result = rep(Double.PositiveInfinity) { hopCount =>
          mux(source)(0.0)(minHoodPlus(nbr(hopCount)) + ctx.action + 1)
        }
        val reward = rewardSignal(result)
        val nextState = state(result, ctx.action)
        val nextAction = pass(nextState, shouldLearn)
        ctx
          .focus(_.output)
          .replace(result)
          .focus(_.action)
          .replace(nextAction)
          .focus(_.state)
          .replace(nextState)
          .focus(_.trajectory)
          .modify(trj => (ctx.state, ctx.action, reward) :: trj.toList)
      }
      (RoundData(q, data.output, data.action), mux(mid() != rightSrc && mid() != leftSrc)(data.trajectory)(Seq.empty))
    }

    def pass(state: List[Int], learn: Boolean): Int = {
      val array = np.array(List(state.toPythonProxy).toPythonProxy)
      val result = if (learn) {
        dqn.sample_action(array)
      } else {
        dqn.predict(array)
      }
      py"$result[0]".as[Int]
    }

    override protected def initialState: List[Int] = List(0, 0)

    override protected def q: Q[List[Int], Int] = MutableQ[List[Int], Int](Map.empty)

    override protected def actionEffect(oldOutput: Double, state: List[Int], action: Int): Double = 0.0

    override def episodeEnd(nodes: Iterable[NodeManager]): Unit = {
      val trajectories = nodes
        .map(_.get[Trajectory[List[Int], Int]]("trajectory_deepRL"))
        .filter(_.nonEmpty)
      val dropped = trajectories.map(_.take(80)).filter(_.size >= 80)
      val correctOrder = dropped.flatMap(_.reverse)
      val states = correctOrder.map(data => np.array(data._1.toPythonProxy)).toList.toPythonProxy
      val actions = correctOrder.map(_._2).toList.toPythonProxy
      val rewards = correctOrder.map(_._3).toList.toPythonProxy
      val terminations = List.tabulate(dropped.size)(_ => List.fill(79)(0) ::: 1 :: Nil).flatten.toPythonProxy
      val dataset =
        d3rlpy.dataset.MDPDataset(np.array(states), np.array(actions), np.array(rewards), np.array(terminations))
      val fitted = dqn.fit(dataset, n_epochs = 50)
    }
  }
}

object TemporalDeepRL {
  lazy val d3rlpy: py.Dynamic = py.module("d3rlpy")
  val dqn: py.Dynamic = d3rlpy.algos.DQN()
  lazy val np: py.Dynamic = py.module("numpy")
  val creation: py.Any = dqn.create_impl(observation_shape = py"(2, )", action_size = 5)
}
