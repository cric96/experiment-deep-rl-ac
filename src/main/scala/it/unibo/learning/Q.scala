package it.unibo.learning
import upickle.default.{macroRW, ReadWriter => RW}

/** a Q value function. For each S,A pair returns the current value
  * @tparam S
  * @tparam A
  */
sealed trait Q[S, A] extends ((S, A) => Double) {
  def update(state: S, action: A, reward: Double): Q[S, A]
  def withDefault(value: => Double): Q[S, A]
}

object Q {
  def zeros[S, A](): Q[S, A] = fillWith(0)
  def fillWith[S, A](value: => Double): Q[S, A] = MutableQ(Map.empty.withDefault(_ => value))

  @SuppressWarnings(Array("org.wartremover.warts.Var")) // because of mutable global state
  case class MutableQ[S, A](var initialConfig: Map[(S, A), Double]) extends Q[S, A] {
    override def apply(state: S, action: A): Double = initialConfig((state, action))
    override def update(state: S, action: A, reward: Double): Q[S, A] = {
      this.initialConfig += (state, action) -> reward
      this
    }
    override def withDefault(value: => Double): Q[S, A] = MutableQ(initialConfig.withDefault(_ => value))
    override def toString(): String = s"MutableQ { map : ${initialConfig.toString()} }"
  }

  @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
  implicit def qRW[S: RW, A: RW]: RW[Q[S, A]] = macroRW[Q[S, A]]
  @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
  implicit def mutableQMap[S: RW, A: RW]: RW[MutableQ[S, A]] = macroRW[MutableQ[S, A]]
}
