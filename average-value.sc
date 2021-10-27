// Dependency
import $ivy.`com.lihaoyi::os-lib:0.7.8`
import $ivy.`com.github.tototoshi::scala-csv:1.3.8`
import $ivy.`org.plotly-scala::plotly-render:0.8.1`

// Proc
import ammonite.ops._
import ammonite.ops.ImplicitWd._

import plotly._
import plotly.element._
import plotly.layout._
import com.github.tototoshi.csv._
import java.io.File
import scala.collection.Factory
implicit object MyFormat extends DefaultCSVFormat {
  override val delimiter = ' '
}

@main
def averageByIndicies(skip: Int, index: Int*): Any = {
  def select[A, F[a] <: Seq[a]](seq: F[A], indicies: Int*)(implicit factory: Factory[A, F[A]]): F[A] = {
    seq.zipWithIndex.filter { case (data, i) => indicies.contains(i) }.map(_._1).to(factory)
  }
  val orderedExperiments =  os.list(os.pwd / "data")
    .filter(os.isFile)
    .sortBy(_.wrapped.toString().split("-").last.toDouble)
  
  val experiments = orderedExperiments
    .map(file => file.wrapped.toAbsolutePath().toString())
    .map(new File(_))
    .map(CSVReader.open)
    .map(_.all())
    .map(file => file.filter(row => row.forall(!_.contains("#"))))
    .drop(skip)
  
  val selectedIndicies = experiments
    .map(
      experiment => experiment.map(row => select(row, index:_*)).map(row => row.map(_.toDouble))
    ).map(
      experiment => 
        
        experiment.reduce((acc, data) => acc.zip(data).map { case (a, b) => a + b} ).map(data => data / experiment.size)
    )
    

  val plotIndicies = experiments.indices.toList
  val plots = selectedIndicies
    .map(element => element.map(List(_)))
    .reduce((acc, data) => acc.zip(data).map { case (acc, data) => acc ::: data})
    .map(Scatter(plotIndicies, _))
    .toSeq
  
  val plotDiff = selectedIndicies.map(row => row.head - row.tail.head)
  val scatter = Scatter(plotIndicies, plotDiff)
  Plotly.plot("id", plots :+ scatter, Layout(title = "Line and Scatter Plot"))
  
}