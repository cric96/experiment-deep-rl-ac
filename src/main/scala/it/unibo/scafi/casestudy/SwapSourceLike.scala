package it.unibo.scafi.casestudy

import it.unibo.Logging
import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.tiggers.EndHandler
import it.unibo.scafi.casestudy.LearningProcess.{RoundData, Trajectory}
import it.unibo.scafi.casestudy.algorithm.RLLike

import scala.jdk.CollectionConverters.IteratorHasAsScala

trait SwapSourceLike extends GradientLike {
  self: RLLike =>
  protected val maxBound = 200
  // Variable loaded by alchemist configuration.
  lazy val leftSrc: Int = node.get[Integer]("left_source") // ID of the source at the left of the env (the stable one)
  lazy val rightSrc: Int =
    node.get[Integer]("right_source") // ID of the source at the right of the env (the unstable one)
  lazy val rightSrcStop: Int =
    node.get[Integer]("stop_right_source") // time at which the source at the right of the env stops being a source

  override def source: Boolean =
    if (mid() == leftSrc || (mid() == rightSrc && passedTime() < rightSrcStop)) true else false

  def algorithms: Seq[AlgorithmTemplate[_, _]]

  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of unsafe scala binding
  override lazy val endHandler: EndHandler[_] = {
    val storeMonitor = new EndHandler[Any](
      sharedLogic = () => {},
      leaderLogic = () => {
        Logging().warn(s"Agents learn? ${learnCondition.toString}")
        Logging().warn(s"Episodes: ${episode.toString}")
        Logging().warn(s"Epsilon: ${epsilon.value(episode).toString}")
        val nodes = alchemistEnvironment.getNodes.iterator().asScala.toList.map(node => new SimpleNodeManager(node))
        algorithms.foreach(_.episodeEnd(nodes))
      },
      id = mid()
    )
    alchemistEnvironment.getSimulation.addOutputMonitor(storeMonitor)
    storeMonitor
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of heterogeneous types
  def processAlgorithms(
      learn: Boolean,
      eps: Double
  ): Map[AlgorithmTemplate[_, _], (RoundData[_, _, Double], Trajectory[_, _])] =
    algorithms.map(l => l -> l.output(learn, eps)).toMap

  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of heterogeneous types
  def storeAllDataFrom(
      reference: Double,
      elements: Map[AlgorithmTemplate[_, _], (RoundData[_, _, Double], Trajectory[_, _])]
  ): Unit =
    elements.foreach { case (algorithm, (data, trajectory)) => store(algorithm, reference, data, trajectory) }

  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of heterogeneous types
  def store(
      algorithm: AlgorithmTemplate[_, _],
      reference: Double,
      data: RoundData[_, _, Double],
      trj: Trajectory[_, _]
  ): Unit = {
    node.put(s"q_${algorithm.name}", algorithm.learningProblem.q)
    node.put(s"output_${algorithm.name}", data.output)
    node.put(s"err_${algorithm.name}", outputEvaluation(reference, data.output))
    node.put(s"action_${algorithm.name}", data.action)
    node.put(s"trajectory_${algorithm.name}", trj)
    node.put(s"reward_${algorithm.name}", trj.headOption.getOrElse(0.0))
  }

  protected def outputEvaluation(ref: Double, value: Double): Double = {

    val result = (ref - value).abs
    if (ref.isInfinite && value.isInfinite) {
      0.0
    } else if (result > maxBound) { alchemistEnvironment.getNodes.size() }
    else { result }
  }
}
