package it.unibo.learning
import upickle.default.{macroRW, ReadWriter => RW}

case class Clock(ticks: Int) {
  def tick: Clock = this.copy(ticks + 1)
}

object Clock {
  def start: Clock = Clock(0)
  implicit val clockRW: RW[Clock] = macroRW[Clock]
}
