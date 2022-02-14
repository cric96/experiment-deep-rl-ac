
// Dependency
import $file.`deps`
import $file.`Utils`
// Proc
import ammonite.ops._
import ammonite.ops.ImplicitWd._
import scala.collection.parallel.CollectionConverters._
// CSV
import com.github.tototoshi.csv._
// Python deps
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.SeqConverters
import java.io.File
import scala.collection.Factory

implicit object MyFormat extends DefaultCSVFormat { override val delimiter = ' ' }

type AnalysisResult = (os.Path, Double, Double)
val acceptedValue = Set("all", "left", "right")
@main
def bestBy(skip: Int, experimentName: String, index: Int, show: Int = 3, division: Int = 6, consider: String = "all"): String = {
  if(!acceptedValue.contains(consider)) {
    throw new IllegalArgumentException(s"consider != ${acceptedValue}")
  }
  val workingDir = os.pwd / "data"
  val suddirs = os.list(workingDir).filter(os.isDir(_)).filterNot(_.wrapped.toAbsolutePath.toString.contains("img"))
  var best = Double.PositiveInfinity // utily...
  def eval(dir: os.Path): Option[(os.Path, Double, Double, Seq[Double])] = {
    val allExperimentFile = Utils.orderedExperiments(dir, experimentName)
    val nonDroppedExperiments =  Utils.extractData(allExperimentFile)
    val experiments = (consider match {
        case "all" => nonDroppedExperiments
        case "left" | "right" => nonDroppedExperiments
          .zipWithIndex
          .filter { case (_, l) => (l / division) % 2 == (if(consider == "left") { 0 } else { 1 })}.map(_._1)
      }).drop(skip)
    def process(): (os.Path, Double, Double, Seq[Double]) = {
      val selectedIndicies = Utils.selectMeanUsingColumns(experiments, index)
      val allData = selectedIndicies.flatten
      val totalAverageError = allData.reduce(_ + _) / allData.size
      val std = allData
        .reduce { (acc, data) => math.sqrt(math.pow(data - totalAverageError, 2)) } / allData.size

      if(best > totalAverageError) {
        best = totalAverageError
        println(s"New best ${dir}! ${totalAverageError} +- ${std}")
      }
      (dir, totalAverageError, std, allData)
    }
    if(experiments.isEmpty) {
      println(s"Skip: ${dir}")
      None
    } else {
      println(s"Process: ${dir}")
      Some(process())
    }
  }
  val sortedResults = (suddirs :+ workingDir)
    .map(eval)
    .collect { case (Some(data)) => data }
    .sortBy { case (_, mean, std, _) => (mean, std) }
  println("---- Analytics ----")
  def analytics(elements: Seq[((os.Path, Double, Double, _), Int)]) = elements.foreach {
    case ((path, mean, std, _), i) => println(s"${i + 1}° ==> ${path.baseName}; ${mean} +- ${std}")
  }
  val zipped = sortedResults.zipWithIndex
  val bestResult = zipped.take(show)
  val worstResult = zipped.reverse.take(show)
  val median = sortedResults.size / 2
  val medianStart = median - show / 2
  val medianResult = zipped.drop(medianStart).take(show)
  println("Best")
  analytics(bestResult)
  println("Worst")
  analytics(worstResult)
  println("Average")
  analytics(medianResult)
  
  // Python part
  val one = bestResult.head._1._4
  val boxplot = one.toPythonCopy
  val plt = py.module("matplotlib.pyplot")
  py.module("matplotlib").rc("figure", figsize = (7, 2))
  def produceBoxPlots(label: String, elements: Seq[((os.Path, _, _, Seq[Double]), _)]): Unit = {
    val boxPlot = elements.map { case ((_, _, _, data), _) => data.toPythonCopy }.toPythonCopy
    val ticks = elements.map { case ((dir, _, _, _), _) => s"${dir.baseName}" }
    plt.boxplot(boxPlot, notch=true, showmeans=true, labels = ticks.toPythonCopy)
    //plt.xticks((0 to ticks.size).toPythonCopy, ("" +: ticks).toPythonCopy)
    plt.ylabel("episode error")
    plt.xlabel("configuration")
    plt.title(s"${label}: box plots")
    os.makeDir.all(os.pwd / "analyse")
    plt.savefig(s"analyse/${label}.pdf")
    plt.clf()
  }
  produceBoxPlots("best", bestResult)
  produceBoxPlots("worst", worstResult)
  produceBoxPlots("median", medianResult)
  produceBoxPlots("all", (bestResult :++ medianResult) :++ worstResult)
  
  sortedResults.headOption.map(_._1.baseName).getOrElse("No file processed")
}