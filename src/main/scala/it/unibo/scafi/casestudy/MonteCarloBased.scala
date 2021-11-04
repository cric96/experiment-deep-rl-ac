package it.unibo.scafi.casestudy

import it.unibo.learning.MonteCarlo

trait MonteCarloBased {
  self: SwapSourceLike =>
  lazy val learningAlgorithm: MonteCarlo.Type[State, Action] =
    MonteCarlo.FirstVisit[State, Action](actions, gamma)
}
