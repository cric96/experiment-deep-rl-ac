package it.unibo.scafi.casestudy

import it.unibo.alchemist.tiggers.EndHandler
import it.unibo.learning.Q
import it.unibo.scafi.casestudy.LearningProcess.RoundData

class SwapSourceOnline extends SwapSourceLike with SarsaBased {
  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of unsafe scala binding
  override lazy val qId: String = mid().toString
  // Aggregate program
  override def aggregateProgram(): RoundData[State, Action, Double] = {
    val classicHopCount = hopGradient(source) // BASELINE
    val hopCountWithoutRightSource =
      hopGradient(mid() == leftSrc) // optimal gradient when RIGHT_SRC stops being a source
    val refHopCount = if (passedTime() >= rightSrcStop) hopCountWithoutRightSource else classicHopCount
    // Learning definition
    val learningProblem = learningProcess(q)
      .stateDefinition(stateFromWindow)
      .rewardDefinition(output => rewardSignal(refHopCount.toInt, output))
      .actionEffectDefinition((output, action) => output + action + 1)
      .initialConditionDefinition(List.empty, Double.PositiveInfinity)
    // RL Program execution
    val (roundData, trajectory) = mux(learnCondition && !source) {
      learningProblem.learn(learningAlgorithm, epsilon.value(episode))
    } {
      learningProblem.actGreedy(learningAlgorithm)
    }
    val stateOfTheArt = crfGradient(1)(source = source, () => 1)
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
    node.put(s"err_rlbasedHopCount", Math.abs(refHopCount - roundData.output))
    node.put(s"passed_time", passedTime())
    node.put("src", source)
    node.put("action", roundData.action)
    node.put(s"refHopCount", Math.abs(refHopCount - stateOfTheArt))
    node.put("trajectory", trajectory)
    roundData
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of unsafe scala binding
  override lazy val endHandler: EndHandler[_] = {
    val storeMonitor = new EndHandler[Any](
      sharedLogic = () => qTableStorage.save(qId, node.get[Q[List[Int], Int]]("qtable")),
      leaderLogic = () => println(s"Episodes: ${episode.toString}"),
      id = mid()
    )
    alchemistEnvironment.getSimulation.addOutputMonitor(storeMonitor)
    storeMonitor
  }
}
