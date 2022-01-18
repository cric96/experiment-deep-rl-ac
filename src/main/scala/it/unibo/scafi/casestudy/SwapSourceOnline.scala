package it.unibo.scafi.casestudy

import cats.data.NonEmptySet
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.{ID, Metric}
import it.unibo.alchemist.tiggers.EndHandler
import it.unibo.learning.QLearning
import it.unibo.scafi.casestudy.CrfLikeDefinition.State

class SwapSourceOnline extends SwapSourceLike {
  // Constants
  val maxValue = 5
  val maxDiff = 100
  val maxUpdateVelocity = 2
  val hopCountMetric: Metric = () => 1
  val hopRadius = 1
  /// Learning definition
  // Plain RL
  lazy val learningAlgorithm: QLearning.Hysteretic[PlainRLDefinition.State, PlainRLDefinition.Action] =
    QLearning.Hysteretic(actions, alpha.value(episode), beta.value(episode), gamma)
  lazy val windowDifferenceSize: Int = node.get[java.lang.Integer]("window")
  lazy val trajectorySize: Int = node.get[java.lang.Integer]("trajectory")
  // CRF Like RL
  lazy val crfLikeLearning: QLearning.Hysteretic[State, CrfLikeDefinition.Action] =
    QLearning.Hysteretic(
      CrfLikeDefinition.actionSpace(maxUpdateVelocity),
      alpha.value(episode),
      beta.value(episode),
      gamma
    )
  // Alchemist molecules
  lazy val actions: NonEmptySet[PlainRLDefinition.Action] = node.get("actions")
  lazy val radius: Double = node.get("range")
  lazy val shouldLearn: Boolean = learnCondition && !source

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
    ///// LEARNING PROBLEMS DEFINITION
    // Crf like learning definition
    val crfProblem = learningProcess(GlobalQ.crfLikeQ)
      .stateDefinition(data => crfLikeState(data, maxValue))
      .rewardDefinition(out => rewardSignal(refHopCount.toInt, out))
      .actionEffectDefinition((output, _, action) => crfActionEffectLike(output, action))
      .initialConditionDefinition(State(None, None), Double.PositiveInfinity)
    // Old learning definition
    val learningProblem = learningProcess(GlobalQ.standardQ)
      .stateDefinition(plainStateFromWindow)
      .rewardDefinition(output => rewardSignal(refHopCount.toInt, output))
      .actionEffectDefinition((output, _, action) => minHoodPlus(nbr(output)) + action + 1)
      .initialConditionDefinition(List.empty, Double.PositiveInfinity)
    // RL Progression
    val (plainLearningResult, trajectory) = learningProblem.step(learningAlgorithm, eps, shouldLearn)
    val (crfLikeLearningResult, _) = crfProblem.step(crfLikeLearning, eps, shouldLearn)
    //// STATE OF THE ART
    val crf = crfGradient(40.0 / 12.0)(source = source, hopCountMetric)
    val bis = bisGradient(hopRadius)(source, hopCountMetric)
    //// ERROR ESTIMATION COUNT
    val rlBasedError = refHopCount - plainLearningResult.output
    val overEstimate =
      if (rlBasedError > 0) { 1 }
      else { 0 }
    val underEstimate =
      if (rlBasedError < 0) { 1 }
      else { 0 }
    //// DATA STORAGE
    node.put("qtable", plainLearningResult.q)
    node.put("classicHopCount", classicHopCount)
    node.put("rlbasedHopCount", plainLearningResult.output)
    node.put(s"passed_time", passedTime())
    node.put("src", source)
    node.put("action", plainLearningResult.action)
    node.put("trajectory", trajectory)
    node.put(s"err_classicHopCount", Math.abs(refHopCount - classicHopCount))
    node.put(s"err_rlbasedHopCount", Math.abs(refHopCount - crfLikeLearningResult.output))
    node.put(s"err_crf", Math.abs(refHopCount - crf.toInt))
    node.put(s"err_bis", Math.abs(refHopCount - bis.toInt))
    node.put(s"err_oldRl", Math.abs(refHopCount - plainLearningResult.output))
    node.put("overestimate", overEstimate)
    node.put("underestimate", underEstimate)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of unsafe scala binding
  override lazy val endHandler: EndHandler[_] = {
    val storeMonitor = new EndHandler[Any](
      sharedLogic = () => {},
      leaderLogic = () => {
        println(s"Agents learn? ${learnCondition.toString}")
        println(s"Episodes: ${episode.toString}")
        println(s"Epsilon: ${epsilon.value(episode).toString}")
      },
      id = mid()
    )
    alchemistEnvironment.getSimulation.addOutputMonitor(storeMonitor)
    storeMonitor
  }

  protected def plainStateFromWindow(output: Double): PlainRLDefinition.State = {
    val minOutput = minHood(nbr(output))
    val recent = recentValues(windowDifferenceSize, minOutput)
    val oldState = recent.headOption.getOrElse(minOutput)
    val diff = (minOutput - oldState) match {
      case diff if Math.abs(diff) > maxDiff => maxDiff * diff.sign
      case diff                             => diff
    }
    recentValues(trajectorySize, diff).toList.map(_.toInt)
  }

  protected def crfLikeState(output: Double, maxValue: Double): CrfLikeDefinition.State = {
    val other = excludingSelf.reifyField(nbr(output))
    val differences = other.map { case (k, v) => k -> (output - v) }
    def align(option: Option[(ID, Double)]): Option[Int] = option
      .map(_._2)
      .map(diff =>
        if (diff.abs > maxValue) { maxValue * diff.sign }
        else { diff }
      )
      .map(_.toInt)

    val left = align(differences.find(_._1 < mid()))
    val right = align(differences.find(_._1 > mid()))
    CrfLikeDefinition.State(left, right)
  }

  protected def crfActionEffectLike(output: Double, action: CrfLikeDefinition.Action): Double = {
    if (action.ignoreLeft && action.ignoreRight) {
      output + action.upVelocity
    } else {
      val data = excludingSelf.reifyField(nbr(output))
      val left = data.find(data => data._1 < mid() && !action.ignoreLeft).map(_._2)
      val right = data.find(data => data._1 > mid() && !action.ignoreRight).map(_._2)
      val minValue =
        List(left, right).collect { case Some(data) => data }.minOption.getOrElse(Double.PositiveInfinity)
      minValue + 1
    }
  }

  protected def rewardSignal(groundTruth: Double, currentValue: Double): Double =
    if ((groundTruth.toInt - currentValue.toInt) == 0) { 0 }
    else { -1 }

}
