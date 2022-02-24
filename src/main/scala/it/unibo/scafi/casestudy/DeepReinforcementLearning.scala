package it.unibo.scafi.casestudy

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.{PyQuote, SeqConverters}

import scala.util.Random

object DeepReinforcementLearning {
  trait Action
  case object ConsiderNeighbourhood extends Action
  case class IncreaseWith(velocity: Double) extends Action
  val supportedAction = Seq(ConsiderNeighbourhood, IncreaseWith(20.0))
  lazy val d3rlpy = py.module("d3rlpy")
  val dqn = d3rlpy.algos.DiscreteSAC()
  lazy val np = py.module("numpy")
  val creation = dqn.create_impl(observation_shape = py"(2, )", action_size = supportedAction.size)

  def pass(seq: Seq[Double], learn: Boolean): (Int, Action) = {
    val array = np.array(List(seq.toPythonProxy).toPythonProxy)

    val index = if (learn) {
      //Random.shuffle(supportedAction.indices.toList).head
      val result = dqn.sample_action(array)
      py"$result[0]".as[Int]
    } else {
      val result = dqn.predict(array)
      py"$result[0]".as[Int]
    }
    (index, supportedAction(index))
  }
}
