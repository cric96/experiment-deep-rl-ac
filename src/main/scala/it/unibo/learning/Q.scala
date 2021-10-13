package it.unibo.learning
import upickle.default.{macroRW, ReadWriter => RW}

sealed trait Q[S, A] extends ((S, A) => Double) {
  def update(state: S, action: A, reward: Double): Q[S, A]
}

object Q {
  def zeros[S, A](): Q[S, A] = fillWith(0)
  def fillWith[S, A](value: => Double): Q[S, A] = QMap(Map.empty.withDefault(_ => value))

  case class QMap[S, A](map: Map[(S, A), Double]) extends Q[S, A] {
    override def apply(state: S, action: A): Double = map((state, action))
    override def update(state: S, action: A, reward: Double): Q[S, A] = copy(map = map + ((state, action) -> reward))
  }

  @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
  implicit def qRW[S: RW, A: RW]: RW[Q[S, A]] = macroRW[Q[S, A]]
  @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
  implicit def qMapRW[S: RW, A: RW]: RW[QMap[S, A]] = macroRW[QMap[S, A]]
}
