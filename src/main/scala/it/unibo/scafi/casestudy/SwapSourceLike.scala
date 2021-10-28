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
  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of unsafe scala binding
  override lazy val qId: String = {
    val random = new Random(episode)
    val nodes = alchemistEnvironment.getNodes.iterator().asScala
    val randomId = random.shuffle(nodes.map(_.getId).toVector)
    if (learnCondition) { randomId.apply(mid()).toString }
    else { mid().toString }
  }
  @SuppressWarnings(Array("org.wartremover.warts.Any")) // because of unsafe scala binding
  override lazy val endHandler: EndHandler[_] = {
    val storeMonitor = new EndHandler[Any](
      sharedLogic = () => {
        qTableStorage.save(qId, node.get[Q[List[Int], Int]]("qtable"))
        clockTableStorage.save(mid().toString, node.get[Clock]("clock"))
      },
      leaderLogic = () => println(s"Episodes: ${episode.toString}"),
      id = mid()
    )
    alchemistEnvironment.getSimulation.addOutputMonitor(storeMonitor)
    storeMonitor
  }

  override def source: Boolean =
    if (mid() == leftSrc || (mid() == rightSrc && passedTime < rightSrcStop)) true else false
}
