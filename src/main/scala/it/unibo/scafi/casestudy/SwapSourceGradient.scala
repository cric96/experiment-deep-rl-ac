package it.unibo.scafi.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.Metric
import it.unibo.scafi.casestudy.algorithm.hopcount.HopCountLearningAlgorithms

class SwapSourceGradient extends HopCountLearningAlgorithms with SwapSourceLike {
  // Constants
  val maxCrfValue = 5
  val maxDiff = 100
  val maxUpdateVelocity = 2
  val hopCountMetric: Metric = () => nbrRange()
  val crfRisingSpeed = 40.0 / 12.0
  val globalReward = -100 // not used currently
  /// Learning definition
  // Alchemist molecules
  lazy val radius: Double = node.get("range")
  lazy val shouldLearn: Boolean = learnCondition && !source
  lazy val algorithms: List[AlgorithmTemplate[_, _]] = List()
  // Aggregate program
  override def aggregateProgram(): Unit = {
    ///// BASELINE
    val classic = classicGradient(source)
    ///// OPTIMAL REFERENCE
    val whenRightIsDown =
      classicGradient(mid() == leftSrc) // optimal gradient when RIGHT_SRC stops being a source
    val referenceGradient = if (passedTime() >= rightSrcStop) whenRightIsDown else classic
    val eps = if (learnCondition) { epsilon.value(episode) }
    else { 0.0 }
    node.put("reference", referenceGradient) // because it will be used by the other algorithms
    // RL Progression
    @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of heterogeneous types
    val progression = processAlgorithms(shouldLearn, eps)
    //// STATE OF THE ART
    val crf = crfGradient(crfRisingSpeed)(source = source, hopCountMetric)
    val bis = bisGradient(radius)(source, hopCountMetric)
    //// DATA STORAGE
    /// OUTPUT
    node.put("output_classicHopCount", classic)
    node.put("output_reference", referenceGradient)
    node.put("output_crf", crf)
    node.put("output_bis", bis)
    /// ERROR
    node.put("err_classicHopCount", outputEvaluation(referenceGradient, classic))
    node.put(s"err_crf", outputEvaluation(referenceGradient, crf))
    node.put(s"err_bis", outputEvaluation(referenceGradient, bis))
    /// MISCELLANEOUS
    node.put(s"passed_time", passedTime())
    node.put("src", source)
    /// RL DATA
    storeAllDataFrom(referenceGradient, progression)
  }
}
