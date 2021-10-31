package it.unibo

import it.unibo.learning.Q.QMap
import it.unibo.storage.LocalStorage

object MergeQMaps extends App {
  type Q = QMap[List[Int], Int]
  private val wd = os.pwd / "qtables"
  val localStorage = new LocalStorage[String]("qtables")
  val qMaps = os.list(wd).map(_.baseName).map(localStorage.load[Q](_))
  localStorage.save("qmap", QMap.merge(qMaps: _*))
}
