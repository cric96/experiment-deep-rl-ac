
import $file.`deps`
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