package it.unibo.storage
import cats.Show
import upickle.default.{ReadWriter => RW}

class LocalStorage[K: Show](globalName: String) {
  private val wd = os.pwd / globalName
  def load[D: RW](key: K): D =
    upickle.default.read[D](os.read(wd / Show[K].show(key)))

  def loadOrElse[D: RW](key: K, orElse: => D): D = if (os.exists(wd / Show[K].show(key))) {
    load(key)
  } else {
    orElse
  }
  def save[D: RW](key: K, data: D): Unit = {
    os.makeDir.all(wd)
    os.write.over(wd / Show[K].show(key), upickle.default.write(data))
  }
  def clean(): Unit = os.remove.all(wd)
}
