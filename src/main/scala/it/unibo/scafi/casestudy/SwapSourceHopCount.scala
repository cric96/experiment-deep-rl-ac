package it.unibo.scafi.casestudy

import cats.data.NonEmptySet
import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.Metric
import it.unibo.alchemist.tiggers.EndHandler
import it.unibo.scafi.casestudy.LearningProcess.{RoundData, Trajectory}
import it.unibo.scafi.casestudy.algorithm.{HopCountLearningAlgorithms, TemporalRL}

import scala.jdk.CollectionConverters.IteratorHasAsScala

class SwapSourceHopCount extends HopCountLearningAlgorithms with SwapSourceLike {
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
  lazy val crfLikeWithActionRL = new CrfLikeWithActionAlgorithm(parameters, maxCrfValue)
  // World Like view
  lazy val worldViewRL = new GlobalViewAlgorithm(parameters, actions)
  // Deep RL
  // lazy val deepRL = new DeepRLAlgorithm(100)
  // Alchemist molecules
  lazy val actions: NonEmptySet[TemporalRL.Action] = node.get("actions")
  lazy val radius: Double = node.get("range")
  lazy val shouldLearn: Boolean = learnCondition && !source
  lazy val algorithms: List[AlgorithmTemplate[_, _]] = List(temporalRL, crfLikeRL, crfLikeWithActionRL /*, deepRL*/ )
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
}
