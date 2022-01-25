package it.unibo

import it.unibo.alchemist.launch.HeadlessSimulationLauncher
import it.unibo.alchemist.loader.LoadAlchemist
import it.unibo.alchemist.loader.providers.YamlProvider
import it.unibo.alchemist.model.interfaces.Position
import org.yaml.snakeyaml.Yaml

import java.io.FileInputStream
import java.util.concurrent.Executors
import java.{util => jutil}
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.SeqHasAsJava
@SuppressWarnings(Array("org.wartremover.warts.All")) //because we have to deal with java world
object MultiLearningRunner extends App {
  implicit class Unsafe(a: Any) {
    def as[T]: T = a.asInstanceOf[T]
    def list: jutil.List[Any] = as[jutil.List[Any]]
    def dict: jutil.Map[AnyRef, Any] = as[jutil.Map[AnyRef, Any]]
  }
  val startingFile = "src/main/yaml/swapSourceGradient.yml"
  val yaml = new Yaml()
  val dir = os.temp.dir(prefix = "simulations")
  val alphaBetaCombination = List((0.5, 0.01), (0.1, 0.01), (0.3, 0.02))
  val epsilonCombination = List((0.9, 10), (0.05, 100), (0.1, 90))
  val bucketsAndMax = List((2, 2), (2, 4), (4, 4), (4, 16), (4, 32))
  def baseYaml = {
    val loader = new FileInputStream(startingFile)
    val result = yaml.load[java.util.Map[String, Object]](loader)
    loader.close()
    result
  }

  val allSimulations = for {
    ((alpha, beta), i) <- alphaBetaCombination.zipWithIndex
    ((epsilon, decay), j) <- epsilonCombination.zipWithIndex
    ((buckets, max), k) <- bucketsAndMax.zipWithIndex
  } yield {
    def suffix = s"$alpha-$beta-$epsilon-$decay-$buckets-$max"
    def suffixNumber = s"$i$j$k"
    val base = baseYaml
    base.put("_beta", s"it.unibo.learning.TimeVariable.independent($beta)")
    base.put("_alpha", s"it.unibo.learning.TimeVariable.independent($alpha)")
    println(base.get("export"))
    base
      .get("export")
      .list
      .get(0)
      .dict
      .put("parameters", List(s"gradientExperiments-$suffix", 1.0, s"./data/$suffixNumber").asJava)
    println(base.get("export"))
    base.put("_epsilon", s" it.unibo.learning.TimeVariable.exponentialDecayFunction($epsilon, $decay)")
    val file = dir / s"sim-$suffix"
    os.write(file, yaml.dump(base))
    LoadAlchemist.from(file.getInputStream, YamlProvider.INSTANCE)
  }
  trait P extends Position[P]
  val execution = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(12))
  allSimulations.foreach(a => execution.execute(() => HeadlessSimulationLauncher.INSTANCE.launch(a, Helper.create())))
}
