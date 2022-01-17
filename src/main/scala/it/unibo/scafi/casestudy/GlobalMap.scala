package it.unibo.scafi.casestudy

import it.unibo.learning.Q.MutableQ

object GlobalMap {
  val q = MutableQ[List[Int], Int](Map.empty.withDefault(_ => 0.0))
}
