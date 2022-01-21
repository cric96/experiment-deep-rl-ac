package it.unibo.scafi.casestudy.algorithm
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.casestudy.{GradientLike, SwapSourceLike}
trait HopCountLearningAlgorithms extends CrfHopCountLikeRL with GlobalViewRL with TemporalRL with TemporalDeepRL {
  self: AggregateProgram with GradientLike with StateManagement with SwapSourceLike =>
}
