package it.unibo.learning
import upickle.default.{macroRW, ReadWriter => RW}

case class Clock(ticks: Long) {
  def tick: Clock = this.copy(ticks + 1)
}

object Clock {
  def start: Clock = Clock(0)
  @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
  implicit val clockRW: RW[Clock] = macroRW[Clock]
}
