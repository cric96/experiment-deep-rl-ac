import $file.`deps`

import ammonite.ops._
// CSV
import com.github.tototoshi.csv._
import java.io.File
import scala.collection.Factory

implicit object MyFormat extends DefaultCSVFormat { override val delimiter = ' ' }

type RawExperiments = Seq[Seq[String]]
type ExperimentsData = Seq[Double]
def orderedExperiments(dir: Path, experimentName: String): Seq[Path] = os.list(dir)
  .filter(os.isFile)
  .filter(_.toString().contains(experimentName))
  .toVector
  .sortBy(file => {
    val numberWithExtension = file.wrapped.toString.split("-").last
    numberWithExtension.split("\\.").head.toDouble
  })


def extractData(orderedExperiments: Seq[Path]): Seq[RawExperiments] = orderedExperiments
  .map(file => file.wrapped.toAbsolutePath.toString)
  .map(new File(_))
  .map(CSVReader.open)
  .map(_.all().toVector.map(_.toVector))
  .map(file => file.filter(row => row.forall(!_.contains("#"))))

    
def selectMeanUsingColumns(experiments: Seq[RawExperiments], indicies: Int*): Seq[ExperimentsData] = {
 experiments
  .map(experiment =>
    experiment
      .map(row => select(row, indicies: _*))
      .map(row => row.map(_.toDouble))
  )
  .map(_.drop(1))
  .map(experiment => {
    experiment
      .reduce((acc, data) => acc.zip(data).map { case (a, b) => a + b })
      .map(data => data / experiment.size)
  })
}


// Utility function
def select[A, F[a] <: Seq[a]](seq: F[A], indicies: Int*)(implicit factory: Factory[A, F[A]]): F[A] = {
  seq.zipWithIndex.filter { case (_, i) => indicies.contains(i) }.map(_._1).to(factory)
}
