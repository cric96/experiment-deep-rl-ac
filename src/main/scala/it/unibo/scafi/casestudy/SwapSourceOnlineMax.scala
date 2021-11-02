package it.unibo.scafi.casestudy

import cats.Show
import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.tiggers.EndHandler
import it.unibo.learning.Clock
import it.unibo.learning.Q.QMap
import it.unibo.scafi.casestudy.LearningProcess.RoundData
import it.unibo.cats.TypeEnrichment._
import scala.jdk.CollectionConverters.IteratorHasAsScala

class SwapSourceOnlineMax extends SwapSourceLike {
  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of unsafe scala binding
  override lazy val qId: String = "global"
  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of unsafe scala binding
  override lazy val endHandler: EndHandler[_] = {
    val storeMonitor = new EndHandler[Any](
      sharedLogic = () => {
        clockTableStorage.save(mid().toString, node.get[Clock]("clock"))
      },
      leaderLogic = () => {
        println(s"episode: ${episode.toString}, epsilon at start: ${epsilon.value(clock).toString}")
        val nodes = alchemistEnvironment.getNodes.iterator().asScala.toList
        println(s"Population size: ${nodes.size.toString}")
        val managers =
          nodes.filter(node => node.getId != rightSrc && node.getId != leftSrc).map(new SimpleNodeManager(_))
        val qtables = managers.map(_.get[QMap[List[Int], Int]]("qtable"))
        val maxQTable = QMap.mergeMax(qtables: _*)
        implicit def listShow[A]: Show[List[A]] = (t: List[A]) => t.mkString(",") // for pretty printing
        qTableStorage.saveRaw("global.csv", QMap.asCsv(maxQTable).getOrElse(""))
        qTableStorage.save(qId, QMap.mergeMax(qtables: _*))
        val states = maxQTable.map.keySet.map(_._1)
        val actionsGreedy =
          states.map(s =>
            s -> actions.map(a => a -> maxQTable.withDefault(initialValue)(s, a)).toNonEmptyList.maxBy(_._2)
          )
        println(actionsGreedy.map { case (s, (a, _)) => (s, a) }.toList.sortBy(_._2).reverse.mkString(";"))
      },
      id = mid()
    )
    alchemistEnvironment.getSimulation.addOutputMonitor(storeMonitor)
    storeMonitor
  }
  // Aggregate program
  override def aggregateProgram(): RoundData[State, Action, Double] = {
    val classicHopCount = hopGradient(source) // BASELINE
    val hopCountWithoutRightSource =
      hopGradient(mid() == leftSrc) // optimal gradient when RIGHT_SRC stops being a source
    val refHopCount = if (passedTime >= rightSrcStop) hopCountWithoutRightSource else classicHopCount
    // Learning definition
    val learningProblem = learningProcess(q)
      .stateDefinition(stateFromWindow)
      .rewardDefinition(output => rewardSignal(refHopCount.toInt, output))
      .actionEffectDefinition((output, action) => output + action + 1)
      .initialConditionDefinition(List.empty, Double.PositiveInfinity)
    // RL Program execution
    val (roundData, trajectory) = mux(learnCondition && !source) {
      learningProblem.learn(learningAlgorithm, epsilon, clock)
    } {
      learningProblem.actGreedy(learningAlgorithm, clock)
    }
    val stateOfTheArt = svdGradient()(source = source, () => 1)
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
    node.put("clock", roundData.clock)
    node.put("classicHopCount", classicHopCount)
    node.put("rlbasedHopCount", roundData.output)
    node.put(s"err_classicHopCount", Math.abs(refHopCount - classicHopCount))
    node.put(s"err_rlbasedHopCount", Math.abs(rlBasedError))
    node.put(s"passed_time", passedTime)
    node.put("src", source)
    node.put("action", roundData.action)
    node.put(s"err_flexHopCount", Math.abs(refHopCount - stateOfTheArt))
    node.put("trajectory", trajectory)
    roundData
  }
}
