import $file.`deps`
import $file.`Utils`
import $file.`analyse`
import $file.`bulk-plots`
import $file.`divide-by`
import $file.`average-value`
import $file.`Logger`
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
import py.PyQuote
import ammonite.repl.tools.Util
// Parallel process
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration
@main
def main(skip: Int) {
  Logger.log = false // other scripts do not produce logs
  val currentFolder = os.pwd
  val result = currentFolder / "result"
  if(os.exists(result)) { os.remove.all(result) }
  val name = "gradientExperiments"
  val pythonPlotConfig = "src/main/plot/swapSourceSamplingPaper.yml"
  val jumpFirst = 10
  val rl = 10
  val show = 3
  val division = 6
  def moveTo(ref: Path, to: Path) = os.move(ref, to, replaceExisting = true, createFolders = true)
  def copyTo(ref: Path, to: Path) = os.copy(ref, to, replaceExisting = true)
  
  val analyseRest = Future {
    println("analysis starts...")
    val best = analyse.bestBy(skip, name, rl, show, division, "all")
    moveTo(currentFolder / "analyse", result / "analyse-all")
    analyse.bestBy(skip, name, rl, show, division, "right")
    moveTo(currentFolder / "analyse", result / "analyse-right")
    analyse.bestBy(skip, name, rl, show, division, "left")
    moveTo(currentFolder / "analyse", result / "analyse-left")  
    println("analysis ends..")
    best
  }
  val average = Future {
    println("average starts..")
    `average-value`.averageByIndicies(jumpFirst, name, 6, "9:classic", "10:RL", "14:CRF")
    println("average ends..")
  }
  val divide = Future {
    println("divide starts..")
    `divide-by`.divideBy(name, division)
    println("divide ends..")
  }
  val best = Await.result(average.flatMap(_ => analyseRest), Duration.Inf)
  val tmp = os.pwd / "temp"
  os.makeDir.all(tmp)
  val bestExp = currentFolder / "data" / best
  moveTo(bestExp / "imgs", result / "imgs-best")
  val bestLeft = Utils.orderedExperiments(bestExp / "left", name).drop(skip / 2)
  bestLeft.foreach(path => copyTo(path, tmp / path.baseName))
  os.proc("python", "src/main/plot/plotter.py", pythonPlotConfig, "temp/", s""".*${name}.*""", "few-nodes").call()
  moveTo(tmp / "imgs", result / "imgs-few")
  os.remove.all(tmp)
  os.makeDir.all(tmp)
  val bestRigth = Utils.orderedExperiments(bestExp / "rigth", name).drop(skip / 2)
  bestRigth.foreach(path => copyTo(path, tmp / path.baseName))
  os.proc("python", "src/main/plot/plotter.py", pythonPlotConfig, "temp/", s""".*${name}.*""", "many-nodes").call()
  moveTo(tmp / "imgs", result / "imgs-many")
  os.remove.all(tmp)
}