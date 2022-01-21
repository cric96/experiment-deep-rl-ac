package it.unibo.scafi.casestudy.algorithm.hopcount

import it.unibo.scafi.casestudy.{GradientLike, SwapSourceLike}
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
trait HopCountLearningAlgorithms extends CrfHopCountLikeRL with GlobalViewRL with TemporalRL with TemporalDeepRL {
  self: AggregateProgram with GradientLike with StateManagement with SwapSourceLike =>
}
