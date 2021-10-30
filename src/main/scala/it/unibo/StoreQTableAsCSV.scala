package it.unibo

import it.unibo.learning.Q.QMap
import it.unibo.storage.LocalStorage

import scala.util.{Success, Try}

object StoreQTableAsCSV extends App {
  type Q = QMap[List[Int], Int]
  private val wd = os.pwd / "qtables"
  val localStorage = new LocalStorage[String]("qtables")
  val csvs = new LocalStorage[String]("csv")
  val qMaps = os.list(wd).map(_.baseName).map(id => Try(localStorage.load[Q](id))).collect { case Success(q) => q }

  println(
    qMaps
      .map(_.map)
      .map(q =>
        q.map { case ((state, action), value) => s"(${state.mkString(";")}) ${action.toString} ${value.toString}" }
      )
      .map(q => q.mkString("\n"))
      .zipWithIndex
      .map { case (data, i) => csvs.saveRaw(s"${i.toString}.csv", data) }
  )
}
