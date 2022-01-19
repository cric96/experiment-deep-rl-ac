package it.unibo.scafi.casestudy
/** Unrealistic scenarios where node can see all the other agents. */
object GlobalView {
  type State = List[Int] // all the output
  type Action = CrfLikeDefinition.Action
}
