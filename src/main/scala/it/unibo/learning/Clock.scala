package it.unibo.learning

case class Clock(ticks: Int) {
  def tick: Clock = this.copy(ticks + 1)
}

object Clock {
  def start: Clock = Clock(0)
}
