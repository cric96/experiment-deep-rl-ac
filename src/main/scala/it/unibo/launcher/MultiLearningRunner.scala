package it.unibo.launcher

import ch.qos.logback.classic.Level
import it.unibo.Logging
import it.unibo.alchemist.Alchemist
import it.unibo.learning.Q.MutableQ
import it.unibo.scafi.casestudy.GlobalStore
import it.unibo.scafi.casestudy.algorithm.gradient.TemporalGradientRL.{Action, History}
import org.slf4j.LoggerFactory
import org.yaml.snakeyaml.Yaml
import upickle.default.read

import java.io.FileInputStream
import java.util.concurrent.{CountDownLatch, Executors, Semaphore}
import scala.collection.compat.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.jdk.CollectionConverters.{CollectionHasAsScala, MapHasAsJava, SeqHasAsJava}

/** Main used to run multiple simulation with reinforcement learning. It could be configured using a file or via main
  * args. The file should be structured as: ["key", "value", "key-2", "value-2", "key-3", "value-3"] The accepted key
  * are: "greedyEpisodes", "10", "gamma", "[0.9]", "alphaBeta", "[[0.5,0.01]]", "epsilonCombination",
  * "[[0.5,50],[0.01,40]]", "bucketsMax", "[[32,4]]"]
  *   - program: with the base alchemist configuration file. e.g "program", "test.yaml"
  *   - learningEpisodes: how many episodes are used to train the Q table?. e.g "learningEpisodes", "100"
  *   - greedyEpisodes: in how many episodes the agent follow a greedy policy? e.g. "greedyEpisodes", "10"
  *   - gamma: value for Q learning. e.g "gamma", [0.9, 0.1] ==> accept an array of double
  *   - alphaBeta: value to tune Hysteric Q learning. e.g. "alphaBeta", "[[0.5, 0.1], [0.1, 0.01]]" ==> accept an array
  *     of tuple
  *   - epsilonCombination: value to rule the epsilon decay. e.g. "epislonCombination", "[[0.5, 50]]" ==> accept an
  *     array of tuple. each tuple expresses [epsilon_0, decay factor]
  *   - bucketsMax: used to tune the discretisation process. "bucketsMax", "[[32, 4]]" ==> accept an array of tuple.
  *     each tuple expresses: [buckets, radiusMultiplier]
  *
  * This will launch |alphaBeta| * |gamma| * |epsilonCombination| * |bucketsMax| simulations
  *
  * An example of that file is in launch.txt.
  *
  * If you use gradle, you can launch these simulation with: ./gradlew startMultipleLearning -PXfile="launch.txt" If you
  * launch this application using java, the argument should be passed as: name value
  */
@SuppressWarnings(Array("org.wartremover.warts.All")) //because we have to deal with java world
object MultiLearningRunner extends App {
  LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).as[ch.qos.logback.classic.Logger].setLevel(Level.WARN)
  val configuration: Seq[String] = {
    if (args.contains("file")) {
      val fileToRead = args(args.indexOf("file") + 1)
      upickle.default.read[List[String]](os.read(os.pwd / fileToRead))
    } else {
      ArraySeq.unsafeWrapArray(args)
    }
  }
  private val yaml = new Yaml()
  private val baseFolder = "src/main/yaml/"
  private val experimentName = findInArgs("program").getOrElse("swapSourceGradientMultiEnvironment.yml")
  private val startingFile = s"$baseFolder$experimentName"
  val dir = os.temp.dir(prefix = "simulations")
  val gamma = findInArgs("gamma").map(read[List[Double]](_)).getOrElse(List(0.9, 0.5, 0.99))
  val alphaBetaCombination =
    findInArgs("alphaBeta").map(read[List[(Double, Double)]](_)).getOrElse(List((0.5, 0.1), (0.1, 0.01)))
  val epsilonCombination = findInArgs("epsilonCombination")
    .map(read[List[(Double, Int)]](_))
    .getOrElse(List((0.01, 1000), (0.03, 1000), (0.1, 500), (0.2, 400)))
  val bucketsAndMax =
    findInArgs("bucketsMax").map(read[List[(Int, Int)]](_)).getOrElse(List((32, 4), (64, 4), (128, 5)))
  val learningEpisodes = findInArgs("learningEpisodes")
  val greedyEpisodes = findInArgs("greedyEpisodes")
  Logging().warn("---- RECAP -----")
  Logging().warn(s"- Program = $startingFile")
  Logging().warn(s"- Gamma = $gamma")
  Logging().warn(s"- Alpha and Beta = $alphaBetaCombination")
  Logging().warn(s"- epsilon = $epsilonCombination")
  Logging().warn(s"- bucket count and max range = $bucketsAndMax")
  Logging().warn(s"- has learning episodes?: ${learningEpisodes.nonEmpty}")
  Logging().warn(s"- has greedy episodes?: ${greedyEpisodes.nonEmpty}")
  Logging().warn("--- END ------")
  val allSimulations = for {
    ((alpha, beta), i) <- alphaBetaCombination.zipWithIndex
    ((epsilon, decay), j) <- epsilonCombination.zipWithIndex
    ((buckets, max), k) <- bucketsAndMax.zipWithIndex
    (gamma, z) <- gamma.zipWithIndex
  } yield {
    def suffix = s"$alpha-$beta-$epsilon-$decay-$buckets-$max-$gamma"
    def suffixNumber = s"$i$j$k$z"
    Logging().warn(s"Prepare: $suffix")
    val base = baseYaml
    val molecules = base.dict.get("deployments").head.dict.get("contents").list.asScala.map(_.dict)
    val variables = base.dict.get("variables").dict
    val exportsSection = base.get("export").list.get(0).dict
    learningEpisodes.foreach { length =>
      variables.dict.get("learning_episodes").dict.put("formula", length.toDouble)
    }
    greedyEpisodes.foreach { length =>
      variables.dict.get("last_greedy_episodes").dict.put("formula", length.toDouble)
    }
    def findMoleculeAndUpdate(name: String, value: String): Unit =
      molecules.filter(_.get("molecule") == name).foreach(_.put("concentration", value))
    /// Put molecules
    findMoleculeAndUpdate("beta", independent(beta))
    findMoleculeAndUpdate("alpha", independent(alpha))
    findMoleculeAndUpdate("epsilon", decayWith(epsilon, decay))
    findMoleculeAndUpdate("buckets", buckets.toDouble.toString)
    findMoleculeAndUpdate("gamma", gamma.toString)
    findMoleculeAndUpdate("maxRadiusMultiplier", max.toDouble.toString)
    // Put simulation maps
    val simulationId = Map("molecule" -> "simulation_id", "concentration" -> s"\"$suffixNumber\"").asJava
    base.dict.get("deployments").head.dict.get("contents").list.add(simulationId)
    exportsSection.put(
      "parameters",
      List(s"gradientExperiments-$suffix-$suffixNumber", 1.0, s"./data/$suffixNumber").asJava
    )
    val file = dir / s"sim-$suffix-$suffixNumber.yml"
    os.write.over(file, yaml.dump(base))
    GlobalStore.store(suffixNumber, new MutableQ[History, Action](Map.empty.withDefault(_ => 0.0)))
    file
  }

  val howMany = Runtime.getRuntime.availableProcessors
  implicit val executionContext: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(howMany))
  val synch = new Semaphore(howMany)
  val latch = new CountDownLatch(allSimulations.size)
  allSimulations
    .foreach { file =>
      synch.acquire()
      Logging().warn(s"Launch :${file.toIO.getName}")
      Future {
        Alchemist.main(Array("-y", file.toString(), "-var", "episode", "-p", "1", "-i", "1", "-hl"))
        synch.release()
        latch.countDown()
        Logging().warn(s"End: $file")
      }
    }
  latch.await()
  System.exit(0)

  private def findInArgs(name: String): Option[String] = {
    configuration.zipWithIndex
      .find(_._1 == name)
      .flatMap { case (_, index) => configuration.zipWithIndex.map { case (v, k) => k -> v }.toMap.get(index + 1) }
  }

  private def baseYaml = {
    val loader = new FileInputStream(startingFile)
    val result = yaml.load[java.util.Map[String, Object]](loader)
    loader.close()
    result
  }

  private def independent(value: Any): String = s"it.unibo.learning.TimeVariable.independent($value)"
  private def decayWith(value: Any, decay: Any): String =
    s"it.unibo.learning.TimeVariable.exponentialDecayFunction($value, $decay)"
}
