
// Dependency
import $ivy.`com.lihaoyi::os-lib:0.7.8`
import $ivy.`com.github.tototoshi::scala-csv:1.3.8`
import $ivy.`org.plotly-scala::plotly-render:0.8.1`
import $ivy.`me.shadaj::scalapy-core:0.5.0`
import $ivy.`com.lihaoyi::ammonite-ops:2.4.1`
import $ivy.`org.scala-lang.modules::scala-parallel-collections:1.0.4`
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

@main
def divideBy(experimentName: String, division: Int): Any = {
  val workingDir = os.pwd / "data"
  val suddirs = os.list(workingDir).filter(os.isDir(_)).filterNot(_.wrapped.toAbsolutePath.toString.contains("img"))
  def eval(dir: os.Path): Unit = {
    println(s"processing ${dir}")
    val orderedExperiments = os.list(dir)
      .filter(os.isFile)
      .filter(_.toString().contains(experimentName))
      .sortBy(file => {
        val numberWithExtension = file.wrapped.toString.split("-").last
        numberWithExtension.split("\\.").head.toDouble
      })
    
    val (left, rigth) = orderedExperiments.zipWithIndex.partition { case (_, l) => (l / division) % 2 == 0 }
    val leftDir = dir / "left"
    val rigthDir = dir / "rigth"
    if(os.exists(leftDir)) { os.remove.all(leftDir) }
    if(os.exists(rigthDir)) { os.remove.all(rigthDir)}
    os.makeDir(leftDir)
    os.makeDir(rigthDir)
    left.foreach { case (data, _) => os.copy(data, leftDir / data.toNIO.toFile.getName()) }
    rigth.foreach { case (data, _) => os.copy(data, rigthDir / data.toNIO.toFile.getName()) }
  }
  eval(workingDir)
  suddirs.foreach(eval)
  "Ok"
}