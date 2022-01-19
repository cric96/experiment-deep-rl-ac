package it.unibo.scafi.casestudy

import it.unibo.learning.Q.MutableQ

/** Usage for managing a single global q table It cannot be used in multithreading context. */
object GlobalQ {
  val standardQ = MutableQ[List[Int], Int](Map.empty.withDefault(_ => 0.0))
  val crfLikeQ = MutableQ[CrfLikeDefinition.State, CrfLikeDefinition.Action](Map.empty.withDefault(_ => 0.0))
  val worldQ = MutableQ[GlobalView.State, GlobalView.Action](Map.empty.withDefault(_ => 0.0))
}
