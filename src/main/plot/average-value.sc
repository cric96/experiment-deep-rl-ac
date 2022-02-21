import $file.`deps`
import $file.`Utils`
import $file.`Logger`
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
import py.PyQuote
implicit object MyFormat extends DefaultCSVFormat { override val delimiter = ' ' }

case class Index(index: Int, label: Option[String] = None)
def processRawString(indices: String*): Seq[Index] = {
  val regex = raw"([0-9]+):(\w+)".r
  val onlyNumber = raw"([0-9]+)".r
  indices.collect { 
    case regex(number, label) => Index(number.toInt, Some(label))
    case onlyNumber(number) => Index(number.toInt)
  }
}
/*
 * Return the time series of average error in each episode for a given index data.
 * Alchemist experiment file are structured as:
 * #######
 * #######
 * col_0     col_1     col_2  ...   col_n
 * data_0_t0 data_1_t0 data_2_t0    data_n_t0
 * data_0_t0 data_1_t0 data_2_t0    data_n_t0
 * .....
 * data_0_tT data_1_tT data_2_tT    data_n_tT
 * where n are the total label extracted, and T is the simulation length.
 * A simulation produces F files (one for each episode).
 * This script, foreach each file, computes the average for a given index set.
 * avg[index] = file[index].reduce(_ + _) / file[index].length
 * where, file[index] produce a view of the file containing only the selected index.
 * For a given index then, the script produces a line plot where x defines the experiment count:
 * ^
 * |
 * |                      x
 * |       x
 * |  x        x  x
 * |                  x              x
 *  _ 0 _ 1 _ 2 _ 3 _ 4 _ 5 _ .... _ F
 *
 * the script should be executed in the same folder where the data/ folder (produced by alchemist) is placed (i.e in the project root).
 * the plots are stored in [exp-folder]/img
 * then, to run the script, you should type:
 * amm src/plot/average-value.sc averageByIndicies --skip n --experimentName name --division d 0 1 2 [ ... ] <- the indices of interests
 * the indicies could be express as:
   - numbers => 1 2 3 
   - number:label => 1:name 2:name 3:name 
 * @param skip the first skip experiment are not computed
 * @param experimentName the name of the experiment
 * @param division used because the experiment in two different configuration. So it tells the period of each environment
 * @param indices what data (selected by index) should be plotted.
 * @return Ok is the simulation goes well.
 */
@main
def averageByIndicies(skip: Int, experimentName: String, division: Int, indices: String*): Any = {
  val indicesParsed = processRawString(indices:_*)
  val selectionColumn = indicesParsed.map(_.index)
  val workingDir = os.pwd / "data"
  val suddirs = os.list(workingDir).filter(os.isDir(_)).filterNot(_.wrapped.toAbsolutePath.toString.contains("img"))
  def createPlotsForExperiments(dir: os.Path): Unit = {
    val imageOutputDir = (dir / "imgs")
    val imageOutputDirPy = imageOutputDir.wrapped.toAbsolutePath.toString

    if(!os.exists(imageOutputDir)){ os.makeDir(imageOutputDir) }

    val allExperimentFile = Utils.orderedExperiments(dir, experimentName)
    val experiments = Utils.extractData(allExperimentFile).zipWithIndex.drop(skip)

    def process(experiments: Seq[Utils.RawExperiments], label: String = ""): Unit = {
      val selectedIndicies = Utils.selectMeanUsingColumns(experiments, selectionColumn:_*)
      val totalError = selectedIndicies.map(_.zipWithIndex)
        .flatMap(_.map { case (k, v) => (v, k)}).groupMapReduce(_._1)(_._2)(_ + _)
      
      Logger.println(s"Stats: ${totalError}")
      
      val labels: Seq[String] = indicesParsed.map { case Index(i, label) => label.getOrElse(i.toString()) }
      val plots = selectedIndicies
        .map(element => element.map(List(_)))
        .reduce((acc, data) => acc.zip(data).map { case (acc, data) => acc ::: data})
        .map(_.toPythonCopy)
        .toSeq.zip(labels)
      
      // Python part
      val plt = py.module("matplotlib.pyplot")
      plt.rcParams.update(py"{'font.size': 14}")
      plots.foreach { case (plot, label) => plt.plot(plot, label=label) }
      //plt.show()
      plt.ylabel("average error")
      plt.xlabel("episodes")
      //plt.title("training errors")
      plt.legend()
      plt.tight_layout()
      plt.savefig(s"$imageOutputDirPy/mean-error-${label}.pdf", bbox_inches="tight", pad_inches = 0)
      plt.clf()
    }
    if(experiments.isEmpty) {
      Logger.println(s"Skip: ${dir}")
    } else {
      Logger.println(s"Process: ${dir}")
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

/*
 * Same as averageByIndicies, but we do not consider the environment division
 * amm src/plot/average-value.sc averageAll --skip n --experimentName name 0 1 2 [ ... ] <- the indices of interests
 */
@main
def averageAll(skip: Int, experimentName: String, indices: String*): Any = {
  averageByIndicies(skip, experimentName, -1, indices:_*)
}
