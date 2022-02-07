package it.unibo.scafi.casestudy

/** A (unsafe) global store used when multiple simulations are deployed */
@SuppressWarnings(Array("org.wartremover.warts.All")) //as a workaround to run multiple simulation in the same jvm
object GlobalStore {
  private var store: Map[Any, Any] = Map.empty[Any, Any]
  def store(id: Any, data: Any): Unit = store += id -> data
  def get[A](id: Any): A = store(id).asInstanceOf[A]
}
