package it.unibo.launcher

import ch.qos.logback.classic.Level
import it.unibo.Logger
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
  private val experimentName = findInArgs("program").getOrElse("swapSourceGradientRectangleVariable.yml")
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
  Logger().warn("---- RECAP -----")
  Logger().warn(s"- Program = $startingFile")
  Logger().warn(s"- Gamma = $gamma")
  Logger().warn(s"- Alpha and Beta = $alphaBetaCombination")
  Logger().warn(s"- epsilon = $epsilonCombination")
  Logger().warn(s"- bucket count and max range = $bucketsAndMax")
  Logger().warn(s"- has learning episodes?: ${learningEpisodes.nonEmpty}")
  Logger().warn(s"- has greedy episodes?: ${greedyEpisodes.nonEmpty}")
  Logger().warn("--- END ------")
  val allSimulations = for {
    ((alpha, beta), i) <- alphaBetaCombination.zipWithIndex
    ((epsilon, decay), j) <- epsilonCombination.zipWithIndex
    ((buckets, max), k) <- bucketsAndMax.zipWithIndex
    (gamma, z) <- gamma.zipWithIndex
  } yield {
    def suffix = s"$alpha-$beta-$epsilon-$decay-$buckets-$max-$gamma"
    def suffixNumber = s"$i$j$k$z"
    Logger().warn(s"Prepare: $suffix")
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
      Logger().warn(s"Launch :${file.toIO.getName}")
      Future {
        Alchemist.main(Array("-y", file.toString(), "-var", "episode", "-p", "1", "-i", "1", "-hl"))
        synch.release()
        latch.countDown()
        Logger().warn(s"End: $file")
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
