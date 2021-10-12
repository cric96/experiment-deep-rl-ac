package it.unibo.scafi.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.learning.{Clock, Q, QLearning, TimeVariable}
import it.unibo.scafi.casestudy.LearningProcess.{BuilderFinalizer, LearningContext}

trait HopCountQLearning {
  self: AggregateProgram =>

  def learningProcess[S, A](initialQ: Q[S, A]): LearningProcess.QBuilderTest[S, A, Int] =
    LearningProcess.QBuilderTest(initialQ)

  implicit class HopCountFinalizer[S, A](learningContext: LearningContext[S, A, Int])
      extends BuilderFinalizer[S, A, Int] {
    override def learn(qLearning: QLearning[S, A], epsilon: TimeVariable[Double], initialClock: Clock): (Q[S, A], Int) =
      ???

    override def act(): (Q[S, A], Int) = ???
  }
}
