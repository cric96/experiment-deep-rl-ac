import $file.`deps`
import $file.`Utils`
// Proc
import ammonite.ops._
import ammonite.ops.ImplicitWd._
// CSV
import com.github.tototoshi.csv._
// Python deps
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.SeqConverters
import java.io.File
import scala.collection.Factory

implicit object MyFormat extends DefaultCSVFormat { override val delimiter = ' ' }

@main
def averageByIndicies(skip: Int, experimentName: String, division: Int, indices: Int*): Any = {
  val workingDir = os.pwd / "data"
  val suddirs = os.list(workingDir).filter(os.isDir(_)).filterNot(_.wrapped.toAbsolutePath.toString.contains("img"))
  def createPlotsForExperiments(dir: os.Path): Unit = {
    val imageOutputDir = (dir / "imgs")
    val imageOutputDirPy = imageOutputDir.wrapped.toAbsolutePath.toString

    if(!os.exists(imageOutputDir)){ os.makeDir(imageOutputDir) }

    val allExperimentFile = Utils.orderedExperiments(dir, experimentName)
    val experiments = Utils.extractData(allExperimentFile).zipWithIndex.drop(skip)

    def process(experiments: Seq[Utils.RawExperiments], label: String = ""): Unit = {
      val selectedIndicies = Utils.selectMeanUsingColumns(experiments, indices:_*)
      val totalError = selectedIndicies.map(_.zipWithIndex)
        .flatMap(_.map { case (k, v) => (v, k)}).groupMapReduce(_._1)(_._2)(_ + _)
      
      println(s"Stats: ${totalError}")
      val plotIndicies = experiments.indices.toList
      val plots = selectedIndicies
        .map(element => element.map(List(_)))
        .reduce((acc, data) => acc.zip(data).map { case (acc, data) => acc ::: data})
        .map(_.toPythonCopy)
        .toSeq
      
      // Python part
      val plt = py.module("matplotlib.pyplot")
      py.module("matplotlib").rc("figure", figsize = (7, 2))
      plots.foreach(plot => plt.plot(plot))
      //plt.show()
      plt.ylabel("average error")
      plt.xlabel("episodes")
      plt.title("training errors")
      plt.savefig(s"$imageOutputDirPy/mean-error-${label}.pdf")
      plt.clf()
    }
    if(experiments.isEmpty) {
      println(s"Skip: ${dir}")
    } else {
      println(s"Process: ${dir}")
      process(experiments.map(_._1), "all") // all 
      if(division > 0) {  
        val (left, right) = experiments.partition { case (_, l) => (l / division) % 2 == 0 }
        process(left.map(_._1), "left")
        process(right.map(_._1), "right")
      }
    }
  }
  createPlotsForExperiments(workingDir)
  suddirs.foreach(createPlotsForExperiments)
  "Ok"
}
@main
def averageAll(skip: Int, experimentName: String, indices: Int*): Any = {
  averageByIndicies(skip, experimentName, -1, indices:_*)
}
