package it.unibo.scafi.casestudy
import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.tiggers.EndHandler
import it.unibo.learning.{Clock, Q}

import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.Random

trait SwapSourceLike extends HopCountLike {
  // Variable loaded by alchemist configuration.
  lazy val leftSrc: Int = node.get[Integer]("left_source") // ID of the source at the left of the env (the stable one)
  lazy val rightSrc: Int =
    node.get[Integer]("right_source") // ID of the source at the right of the env (the unstable one)
  lazy val rightSrcStop: Int =
    node.get[Integer]("stop_right_source") // time at which the source at the right of the env stops being a source

  override def source: Boolean =
    if (mid() == leftSrc || (mid() == rightSrc && passedTime < rightSrcStop)) true else false
}
