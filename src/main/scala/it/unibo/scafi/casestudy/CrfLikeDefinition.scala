package it.unibo.scafi.casestudy

import cats.data.NonEmptySet
import upickle.default.{macroRW, ReadWriter => RW}
import scala.collection.immutable.SortedSet
import scala.language.reflectiveCalls

/** Module that contains the main definition for a CRF like setting in RL */
object CrfLikeDefinition {
  implicit def ordering: Ordering[Action] = Ordering.by(a => (a.ignoreLeft, a.ignoreRight, a.upVelocity))
  case class State(left: Option[Int], right: Option[Int])
  @SuppressWarnings(Array("org.wartremover.warts.All")) // because fast check
  case class Action(ignoreLeft: Boolean = false, ignoreRight: Boolean = false, upVelocity: Int = 0)

  def actionSpace(velocities: List[Int]): NonEmptySet[Action] = {
    val booleanValues = List(false, true)
    val elements = velocities
    val actionSpace = for {
      left <- booleanValues
      right <- booleanValues
      up <- elements
    } yield Action(left, right, up)
    NonEmptySet.fromSetUnsafe(SortedSet(actionSpace: _*))
  }
  @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
  implicit def neighborhood: RW[State] = macroRW[State]
  @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
  implicit def action: RW[Action] = macroRW[Action]
}
