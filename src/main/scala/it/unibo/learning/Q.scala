package it.unibo.learning
import cats.Show
import upickle.default.{macroRW, ReadWriter => RW}
import monocle.syntax.all._
sealed trait Q[S, A] extends ((S, A) => Double) {
  def update(state: S, action: A, reward: Double): Q[S, A]
  def withDefault(value: => Double): Q[S, A]
}

object Q {
  def zeros[S, A](): Q[S, A] = fillWith(0)
  def fillWith[S, A](value: => Double): Q[S, A] = QMap(Map.empty.withDefault(_ => value))

  @SuppressWarnings(Array("org.wartremover.warts.All")) // because fast check
  case class MutableQ[S, A](var initialConfig: Map[(S, A), Double]) extends Q[S, A] {
    override def apply(state: S, action: A): Double = initialConfig((state, action))
    override def update(state: S, action: A, reward: Double): Q[S, A] = {
      this.initialConfig += (state, action) -> reward
      this
    }
    override def withDefault(value: => Double): Q[S, A] = MutableQ(initialConfig.withDefault(_ => value))
    override def toString(): String = s"MutableQ { map : ${initialConfig.toString()} }"
  }

  case class QMap[S, A](map: Map[(S, A), Double]) extends Q[S, A] {
    override def apply(state: S, action: A): Double = map((state, action))
    override def update(state: S, action: A, reward: Double): Q[S, A] =
      this.focus(_.map).modify(_ + ((state, action) -> reward))
    override def withDefault(value: => Double): Q[S, A] = QMap(map.withDefault(_ => value))
    override def toString(): String = s"QMap { map : ${map.toString()} }"
  }

  object QMap {
    def merge[S, A](qMaps: QMap[S, A]*): QMap[S, A] = {
      val total = qMaps.length
      val keys = qMaps.flatMap(qMap => qMap.map.keySet).toSet
      val mergedMap = qMaps
        .map(_.map)
        .flatMap(map =>
          keys
            .map(key => (key, map.get(key)))
            .collect { case (key, Some(value)) => (key, value) }
        )
        .groupMapReduce(_._1)(_._2)(_ + _)
        .map { case (k, qValue) => (k, qValue / total) }
      QMap(mergedMap)
    }
    def mergeMax[S, A](qMaps: QMap[S, A]*): QMap[S, A] = {
      val keys = qMaps.flatMap(qMap => qMap.map.keySet).toSet
      val mergedMap = qMaps
        .map(_.map)
        .flatMap(map =>
          keys
            .map(key => (key, map.get(key)))
            .collect { case (key, Some(value)) => (key, value) }
        )
        .groupMapReduce(_._1)(_._2)(Math.max)
      QMap(mergedMap)
    }
    def asCsv[S: Show, A: Show](q: Q[S, A]): Option[String] = q match {
      case QMap(q) =>
        Some(
          q.map { case ((state, action), value) =>
            s"(${Show[S].show(state)}) ${Show[A].show(action)} ${value.toString}"
          }.mkString("\n")
        )
      case _ => None
    }
  }
  @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
  implicit def qRW[S: RW, A: RW]: RW[Q[S, A]] = macroRW[Q[S, A]]
  @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
  implicit def qMapRW[S: RW, A: RW]: RW[QMap[S, A]] = macroRW[QMap[S, A]]
  @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
  implicit def mutableQMap[S: RW, A: RW]: RW[MutableQ[S, A]] = macroRW[MutableQ[S, A]]
}
