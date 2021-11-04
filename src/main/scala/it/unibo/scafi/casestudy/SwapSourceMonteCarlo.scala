package it.unibo.scafi.casestudy

import cats.Show
import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.tiggers.EndHandler
import it.unibo.learning.Q.QMap
import it.unibo.learning.{Clock, MonteCarlo, Policy, TimeVariable}
import it.unibo.scafi.casestudy.LearningProcess.RoundData

import scala.jdk.CollectionConverters.IteratorHasAsScala

class SwapSourceMonteCarlo extends SwapSourceLike with MonteCarloBased {
  override lazy val qId: String = "global"
  val factor = 100
  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of unsafe scala binding
  override lazy val endHandler: EndHandler[_] = {
    val storeMonitor = new EndHandler[Any](
      sharedLogic = () => {
        clockTableStorage.save(mid().toString, node.get[Clock]("clock"))
      },
      leaderLogic = () => {
        val epsilon =
          TimeVariable.exponentialDecayFunction(zeroBasedEpsilon, factor).value(Clock(episode))
        println(s"Episodes: ${episode.toString}, epsilon: ${epsilon.toString}")
        val defaultEstimation: Map[(State, Action), (Double, Int)] =
          Map.empty[(State, Action), (Double, Int)].withDefaultValue((0.0, 0))
        val estimations = qTableStorage.loadOrElse("estimation", defaultEstimation)
        val nodes = alchemistEnvironment.getNodes.iterator().asScala.toList
        println(s"Population size: ${nodes.size.toString}")
        val managers = nodes.map(new SimpleNodeManager(_))
        val trajectories =
          randomGen.shuffle(managers.map(node => (node.get[Seq[(State, Action, Double)]]("trajectory"))))
        val (qUpdate, estimationUpdate) = trajectories.headOption.foldLeft((q, estimations)) {
          case ((q, estimation), trajectory) =>
            val (qUpdated, trace) = learningAlgorithm.improve(trajectory, (q, estimation), Clock.start)
            (qUpdated, trace)
        }
        implicit def listShow[A]: Show[List[A]] = (t: List[A]) => t.mkString(",") // for pretty printing
        qTableStorage.save("global", qUpdate)
        qTableStorage.saveRaw("global.csv", QMap.asCsv(qUpdate).getOrElse(""))
        qTableStorage.save("estimation", estimationUpdate)
      },
      id = mid()
    )
    alchemistEnvironment.getSimulation.addOutputMonitor(storeMonitor)
    storeMonitor
  }
  lazy val zeroBasedEpsilon = epsilon.value(Clock.start)
  // Aggregate program
  override def aggregateProgram(): RoundData[State, Action, Double] = {
    val classicHopCount = hopGradient(source) // BASELINE
    val hopCountWithoutRightSource =
      hopGradient(mid() == leftSrc) // optimal gradient when RIGHT_SRC stops being a source
    val refHopCount = if (passedTime >= rightSrcStop) hopCountWithoutRightSource else classicHopCount
    // Learning definition
    val problem = learningProblem(refHopCount.toInt)
    // RL Program execution
    val epsilon =
      TimeVariable.exponentialDecayFunction(zeroBasedEpsilon, factor).value(Clock(episode))
    val (roundData, trajectory) = {
      problem.actWith(
        learningAlgorithm.ops,
        clock,
        Policy.softFixedEpsilonGreedy(actions, epsilon)
      )
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
    node.put(s"err_rlbasedHopCount", Math.abs(refHopCount - roundData.output))
    node.put(s"passed_time", passedTime)
    node.put("src", source)
    node.put("action", roundData.action)
    node.put(s"err_flexHopCount", Math.abs(refHopCount - stateOfTheArt))
    node.put("trajectory", trajectory)
    roundData
  }
}
