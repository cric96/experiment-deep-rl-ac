package it.unibo

import it.unibo.MultiLearningRunner.learningEpisodes
import org.yaml.snakeyaml.Yaml

import java.io.FileInputStream
import java.util.concurrent.Executors
import java.{util => jutil}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.CollectionConverters.{CollectionHasAsScala, SeqHasAsJava}
import upickle.default._

@SuppressWarnings(Array("org.wartremover.warts.All")) //because we have to deal with java world
object MultiLearningRunner extends App {
  def findInArgs(name: String): Option[String] =
    args.zipWithIndex
      .find(_._1 == name)
      .flatMap { case (_, index) => args.zipWithIndex.map { case (v, k) => k -> v }.toMap.get(index + 1) }

  def baseYaml = {
    val loader = new FileInputStream(startingFile)
    val result = yaml.load[java.util.Map[String, Object]](loader)
    loader.close()
    result
  }

  implicit class Unsafe(a: Any) {
    def as[T]: T = a.asInstanceOf[T]
    def list: jutil.List[Any] = as[jutil.List[Any]]
    def dict: jutil.Map[AnyRef, Any] = as[jutil.Map[AnyRef, Any]]
    def head: Any = list.get(0)
  }

  val baseFolder = "src/main/yaml/"
  val experimentName = findInArgs("program").getOrElse("swapSourceGradientRectangleVariable.yml")
  val startingFile = s"src/main/yaml/$experimentName"
  val yaml = new Yaml()
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
  println("---- RECAP -----")
  println(s"- Program = $startingFile")
  println(s"- Gamma = $gamma")
  println(s"- Alpha and Beta = $alphaBetaCombination")
  println(s"- epsilon = $epsilonCombination")
  println(s"- bucket count and max range = $bucketsAndMax")
  println(s"- has learning episodes?: ${learningEpisodes.nonEmpty}")
  println(s"- has greedy episodes?: ${greedyEpisodes.nonEmpty}")
  println("--- END ------")
  val allSimulations = for {
    ((alpha, beta), i) <- alphaBetaCombination.zipWithIndex
    ((epsilon, decay), j) <- epsilonCombination.zipWithIndex
    ((buckets, max), k) <- bucketsAndMax.zipWithIndex
    (gamma, z) <- gamma.zipWithIndex
  } yield {
    def suffix = s"$alpha-$beta-$epsilon-$decay-$buckets-$max-$gamma"
    def suffixNumber = s"$i$j$k$z"
    val base = baseYaml
    learningEpisodes.foreach { length =>
      base.dict.get("variables").dict.get("learning_episodes").dict.put("formula", length.toDouble)
    }
    learningEpisodes.foreach { length =>
      base.dict.get("variables").dict.get("last_greedy_episodes").dict.put("formula", length.toDouble)
    }
    val molecules = base.dict.get("deployments").head.dict.get("contents").list.asScala.map(_.dict)
    def findMoleculeAndUpdate(name: String, value: String): Unit =
      molecules.filter(_.get("molecule") == name).foreach(_.put("concentration", value))
    findMoleculeAndUpdate("beta", s"it.unibo.learning.TimeVariable.independent($beta)")
    findMoleculeAndUpdate("alpha", s"it.unibo.learning.TimeVariable.independent($alpha)")
    findMoleculeAndUpdate("epsilon", s"it.unibo.learning.TimeVariable.exponentialDecayFunction($epsilon, $decay)")
    findMoleculeAndUpdate("buckets", buckets.toDouble.toString)
    findMoleculeAndUpdate("gamma", gamma.toString)
    findMoleculeAndUpdate("maxRadiusMultiplier", max.toDouble.toString)
    base
      .get("export")
      .list
      .get(0)
      .dict
      .put("parameters", List(s"gradientExperiments-$suffix", 1.0, s"./data/$suffixNumber").asJava)
    base.put("_epsilon", s" it.unibo.learning.TimeVariable.exponentialDecayFunction($epsilon, $decay)")
    base.put("_buckets", buckets.toString)
    base.put("_maxRadiusMultiplier", max.toString)
    val file = dir / s"sim-$suffix.yml"
    os.write.over(file, yaml.dump(base))
    file
  }
  val saneFactor = 8
  implicit val executionContext =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors / saneFactor))

  allSimulations
    .grouped(Runtime.getRuntime.availableProcessors / saneFactor)
    .map { files =>
      files.map(file =>
        Future {
          println(s"process: $file")
          (
            file,
            os.proc("./gradlew", "startBatchUsing", s"-Pprogram=${file.wrapped.toFile.toString}").call(check = false)
          )
        }
      )
    }
    .foreach { futures =>
      futures.foreach(f => f.onComplete(result => println(result)))
      futures.foreach(f => Await.result(f, Duration.Inf))
    }
  println("End....")
  System.exit(0)
}
