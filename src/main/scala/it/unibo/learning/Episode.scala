package it.unibo.learning
import upickle.default.{macroRW, ReadWriter => RW}

/** data that contains the current episode count
  * @param count
  *   the episode count maintained
  */
case class Episode(count: Long) {
  def tick: Episode = this.copy(count + 1)
}

object Episode {
  /** @return initial configuration for the episode */
  def zero: Episode = Episode(0)
  @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
  implicit val clockRW: RW[Episode] = macroRW[Episode]
}
