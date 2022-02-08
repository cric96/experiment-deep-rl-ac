
// Dependency
import $ivy.`com.lihaoyi::os-lib:0.7.8`
import $ivy.`com.github.tototoshi::scala-csv:1.3.8`
import $ivy.`org.plotly-scala::plotly-render:0.8.1`
import $ivy.`me.shadaj::scalapy-core:0.5.0`
import $ivy.`com.lihaoyi::ammonite-ops:2.4.1`

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
def averageByIndicies(skip: Int, experimentName: String, indices: Int*): Any = {
  val workingDir = os.pwd / "data"
  val suddirs = os.list(workingDir).filter(os.isDir(_)).filterNot(_.wrapped.toAbsolutePath.toString.contains("img"))
  def createPlotsForExperiments(dir: os.Path): Unit = {
    val imageOutputDir = (dir / "imgs")
    val imageOutputDirPy = imageOutputDir.wrapped.toAbsolutePath.toString

    if(!os.exists(imageOutputDir)){ os.makeDir(imageOutputDir) }

    val orderedExperiments = os.list(dir)
      .filter(os.isFile)
      .filter(_.toString().contains(experimentName))
      .sortBy(file => {
        val numberWithExtension = file.wrapped.toString.split("-").last
        numberWithExtension.split("\\.").head.toDouble
      })

    val experiments = orderedExperiments
      .map(file => file.wrapped.toAbsolutePath.toString)
      .map(new File(_))
      .map(CSVReader.open)
      .map(_.all())
      .map(file => file.filter(row => row.forall(!_.contains("#"))))
      .drop(skip)

    def process(): Unit = {
      val selectedIndicies = experiments
        .map(
          experiment => experiment.map(row => select(row, indices:_*)).map(row => row.map(_.toDouble))
        ).map(_.drop(1))
        .map(
          experiment => {
            experiment.reduce((acc, data) => acc.zip(data).map { case (a, b) => a + b} ).map(data => data / experiment.size)
          }
        )
      
      val totalError = selectedIndicies.map(_.zipWithIndex).flatMap(_.map { case (k, v) => (v, k)}).groupMapReduce(_._1)(_._2)(_ + _)

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
      plt.savefig(s"$imageOutputDirPy/mean-error.png")
      plt.clf()
    }
    if(experiments.isEmpty) {
      println(s"Skip: ${dir}")
    } else {
      println(s"Process: ${dir}")
      process()
    }
  }
  createPlotsForExperiments(workingDir)
  suddirs.foreach(createPlotsForExperiments)
  "Ok"
}
// Utility function
def select[A, F[a] <: Seq[a]](seq: F[A], indicies: Int*)(implicit factory: Factory[A, F[A]]): F[A] = {
  seq.zipWithIndex.filter { case (_, i) => indicies.contains(i) }.map(_._1).to(factory)
}