package it.unibo.scafi.casestudy.algorithm
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.casestudy.HopCountLike
trait LearningAlgorithms extends CrfLikeRL with GlobalViewRL with TemporalRL {
  self: AggregateProgram with HopCountLike with StateManagement =>
}
