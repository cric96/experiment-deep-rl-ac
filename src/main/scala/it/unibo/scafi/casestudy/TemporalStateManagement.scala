package it.unibo.scafi.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

import scala.collection.immutable.Queue

trait TemporalStateManagement {
  self: AggregateProgram with FieldUtils =>
  val minWindowsSize = 2
  // TODO: add in ScaFi stdlib
  def delta(value: Double, k: Int, default: Double): Double = {
    require(k > minWindowsSize)
    val vs = recentValues(k, value)
    val deltaResult = for {
      head <- vs.headOption
      last <- vs.take(k).lastOption
    } yield (head - last)
    deltaResult.getOrElse(default)
  }

  // TODO: add in ScaFi stdlib
  def deltas[T: Numeric](value: T, k: Int): Iterable[T] = {
    require(k > minWindowsSize)
    val vs = recentValues(k, value)
    vs match {
      case Queue(_) => List.empty
      case _ =>
        vs.sliding(minWindowsSize)
          .map(_.toList)
          .collect { case head :: last :: _ => implicitly[Numeric[T]].minus(head, last) }
          .toList
    }
  }

  // TODO: add in ScaFi stdlib
  def isIncreasing[T: Ordering](value: T, k: Int, default: Boolean): Boolean = {
    require(k > minWindowsSize)
    val vs = recentValues(k, value)
    vs match {
      case Queue(_) => default
      case _ =>
        vs.sliding(minWindowsSize)
          .map(_.toList)
          .collect { case head :: last :: _ => implicitly[Ordering[T]].compare(head, last) }
          .sum < 0
    }
  }

  // TODO: add in ScaFi stdlib
  def isStable[T](value: T, k: Int): Boolean = {
    require(k > minWindowsSize)
    recentValues(k, value).forall(_ == value)
  }
  // TODO: add in ScaFi stdlib
  def previous[T](value: T): Option[T] =
    recentValues(minWindowsSize, value).dropRight(1).headOption

  // TODO: add in ScaFi stdlib
  def varianceFor(value: Double, k: Int): Double = {
    val vs = recentValues(k, value)
    val len = vs.length
    val mean = vs.sum / len
    vs.foldLeft(0.0)((acc, v) => acc + Math.pow(v - mean, 2)) / len
  }

  // TODO: add in ScaFi stdlib
  def recentValues[T](k: Int, value: T): Queue[T] =
    rep(Queue[T]()) { case (vls) => (if (vls.size == k) vls.drop(1) else vls) :+ value }
}
