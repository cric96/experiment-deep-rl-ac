package it.unibo.scafi.casestudy

import cats.data.NonEmptySet
import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.Metric
import it.unibo.alchemist.tiggers.EndHandler
import it.unibo.scafi.casestudy.LearningProcess.{RoundData, Trajectory}
import it.unibo.scafi.casestudy.algorithm.{LearningAlgorithms, TemporalRL}

import scala.jdk.CollectionConverters.IteratorHasAsScala

class SwapSourceOnline extends LearningAlgorithms with SwapSourceLike {
  // Constants
  val maxCrfValue = 5
  val maxDiff = 100
  val maxUpdateVelocity = 2
  val hopCountMetric: Metric = () => 1
  val hopRadius = 1
  val crfRisingSpeed = 40.0 / 12.0
  val globalReward = -100 // not used currently
  /// Learning definition
  // Plain RL
  lazy val windowDifferenceSize: Int = node.get[java.lang.Integer]("window")
  lazy val trajectorySize: Int = node.get[java.lang.Integer]("trajectory")
  lazy val temporalRL = new TemporalRLAlgorithm(parameters, actions, maxDiff, windowDifferenceSize, trajectorySize)
  // CRF Like RL
  lazy val crfLikeRL = new CrfLikeAlgorithm(parameters, maxCrfValue)
  // World Like view
  lazy val worldViewRL = new GlobalViewAlgorithm(parameters, actions)
  // Alchemist molecules
  lazy val actions: NonEmptySet[TemporalRL.Action] = node.get("actions")
  lazy val radius: Double = node.get("range")
  lazy val shouldLearn: Boolean = learnCondition && !source
  lazy val algorithms: List[AlgorithmTemplate[_, _]] = List(temporalRL, crfLikeRL)
  // Aggregate program
  override def aggregateProgram(): Unit = {
    ///// BASELINE
    val classicHopCount = hopGradient(source)
    ///// OPTIMAL REFERENCE
    val hopCountWithoutRightSource =
      hopGradient(mid() == leftSrc) // optimal gradient when RIGHT_SRC stops being a source
    val refHopCount = if (passedTime() >= rightSrcStop) hopCountWithoutRightSource else classicHopCount
    val eps = if (learnCondition) { epsilon.value(episode) }
    else { 0.0 }
    node.put("reference", refHopCount) // because it will be used by the other algorithms
    // RL Progression
    @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of heterogeneous types
    val progression = processAlgorithms(shouldLearn, eps)
    //// STATE OF THE ART
    val crf = crfGradient(crfRisingSpeed)(source = source, hopCountMetric)
    val bis = bisGradient(hopRadius)(source, hopCountMetric)
    //// DATA STORAGE
    /// OUTPUT
    node.put("output_classicHopCount", classicHopCount)
    node.put("output_reference", refHopCount)
    node.put("output_crf", crf)
    node.put("output_bis", bis)
    /// ERROR
    node.put("err_classicHopCount", outputEvaluation(refHopCount, classicHopCount))
    node.put(s"err_crf", outputEvaluation(refHopCount.toInt, crf.toInt))
    node.put(s"err_bis", outputEvaluation(refHopCount.toInt, bis.toInt))
    /// MISCELLANEOUS
    node.put(s"passed_time", passedTime())
    node.put("src", source)
    /// RL DATA
    storeAllDataFrom(refHopCount, progression)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of unsafe scala binding
  override lazy val endHandler: EndHandler[_] = {
    val storeMonitor = new EndHandler[Any](
      sharedLogic = () => {},
      leaderLogic = () => {
        println(s"Agents learn? ${learnCondition.toString}")
        println(s"Episodes: ${episode.toString}")
        println(s"Epsilon: ${epsilon.value(episode).toString}")
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
    if (result.isInfinite) { alchemistEnvironment.getNodes.size() }
    else { result }
  }
}
