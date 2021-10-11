package it.unibo.learning

trait Q[S, A] extends ((S, A) => Double) {
  def update(state: S, action: A, reward: Double): Q[S, A]
}

object Q {
  def zeros[S, A](): Q[S, A] = fillWith(0)
  def fillWith[S, A](value: => Double): Q[S, A] = QMap(Map.empty.withDefault(_ => value))

  private case class QMap[S, A](map: Map[(S, A), Double]) extends Q[S, A] {
    override def apply(state: S, action: A): Double = map((state, action))
    override def update(state: S, action: A, reward: Double): Q[S, A] = copy(map = map + ((state, action) -> reward))
  }
}