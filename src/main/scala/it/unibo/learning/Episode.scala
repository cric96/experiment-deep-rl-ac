package it.unibo.learning
import upickle.default.{macroRW, ReadWriter => RW}

case class Episode(count: Long) {
  def tick: Episode = this.copy(count + 1)
}

object Episode {
  def start: Episode = Episode(0)
  @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
  implicit val clockRW: RW[Episode] = macroRW[Episode]
}
