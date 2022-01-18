package it.unibo.scafi.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ID
import it.unibo.alchemist.tiggers.EndHandler
import it.unibo.learning.{Q, QLearning}
import it.unibo.scafi.casestudy.CrfLikeDefinition.{State, actionSpace}
import it.unibo.scafi.casestudy.LearningProcess.RoundData

class SwapSourceOnline extends SwapSourceLike with SarsaBased {
  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of unsafe scala binding
  lazy val radius: Double = node.get("range")
  val maxValue = 5
  lazy val crfLikeLearning = QLearning.Hysteretic[CrfLikeDefinition.State, CrfLikeDefinition.Action](
    CrfLikeDefinition.actionSpace(2),
    alpha.value(episode),
    beta.value(episode),
    gamma
  )
  // Aggregate program
  override def aggregateProgram(): RoundData[State, Action, Double] = {
    val classicHopCount = hopGradient(source) // BASELINE
    val hopCountWithoutRightSource =
      hopGradient(mid() == leftSrc) // optimal gradient when RIGHT_SRC stops being a source
    val refHopCount = if (passedTime() >= rightSrcStop) hopCountWithoutRightSource else classicHopCount
    val shouldLearn = learnCondition && !source
    val eps = if (learnCondition) { epsilon.value(episode) }
    else { 0.0 }
    ///// LEARNING PROBLEMS DEFINITION
    // Crf like definition
    val crfProblem = learningProcess(GlobalQ.crfLikeQ)
      .stateDefinition(data => crfLikeState(data, maxValue))
      .rewardDefinition(out => rewardSignal(refHopCount.toInt, out))
      .actionEffectDefinition((output, _, action) => crfActionEffectLike(output, action))
      .initialConditionDefinition(State(None, None), Double.PositiveInfinity)
    // Learning definition
    val learningProblem = learningProcess(GlobalQ.standardQ)
      .stateDefinition(plainStateFromWindow)
      .rewardDefinition(output => rewardSignal(refHopCount.toInt, output))
      .actionEffectDefinition((output, state, action) => minHoodPlus(nbr(output)) + action + 1)
      .initialConditionDefinition(List.empty, Double.PositiveInfinity)
    // RL Program execution
    val (roundData, trajectory) = learningProblem.step(learningAlgorithm, eps, !shouldLearn)
    // Another Rl
    val (roundAnother, _) = crfProblem.step(crfLikeLearning, eps, !shouldLearn)
    val crf = crfGradient(40.0 / 12.0)(source = source, () => 1)
    val bis = bisGradient(1)(source, () => 1)
    val rlBasedError = refHopCount - roundData.output
    val overEstimate =
      if (rlBasedError > 0) { 1 }
      else { 0 }
    val underEstimate =
      if (rlBasedError < 0) { 1 }
      else { 0 }
    // Store alchemist info
    node.put("overestimate", overEstimate)
    node.put("underestimate", underEstimate)
    node.put("qtable", roundData.q)
    node.put("classicHopCount", classicHopCount)
    node.put("rlbasedHopCount", roundData.output)
    node.put(s"err_classicHopCount", Math.abs(refHopCount - classicHopCount))
    node.put(s"err_rlbasedHopCount", Math.abs(refHopCount - roundAnother.output))
    node.put(s"passed_time", passedTime())
    node.put("src", source)
    node.put("action", roundData.action)
    node.put(s"err_crf", Math.abs(refHopCount - crf.toInt))
    node.put(s"err_bis", Math.abs(refHopCount - bis.toInt))
    node.put(s"err_oldRl", Math.abs(refHopCount - roundData.output))
    node.put("trajectory", trajectory)
    roundData
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

  protected def plainStateFromWindow(output: Double): State = {
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
      val haveLeft = data.find(data => data._1 < mid() && !action.ignoreLeft).map(_._2)
      val haveRight = data.find(data => data._1 > mid() && !action.ignoreRight).map(_._2)
      val minValue =
        List(haveLeft, haveRight).collect { case Some(data) => data }.minOption.getOrElse(Double.PositiveInfinity)
      minValue + 1
    }
  }

  protected def rewardSignal(groundTruth: Double, currentValue: Double): Double =
    if ((groundTruth.toInt - currentValue.toInt) == 0) { 0 }
    else { -1 }

}
