package it.unibo.scafi.casestudy

import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.Metric
import it.unibo.alchemist.tiggers.EndHandler
import it.unibo.scafi.casestudy.DeepReinforcementLearning.{ConsiderNeighbourhood, IncreaseWith}
import me.shadaj.scalapy.py.SeqConverters

import scala.collection.immutable.Queue
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.Random

class SwapSourceGradient extends GradientLike {
  // Implicit context variable
  implicit lazy val rand: Random = randomGen
  /// Constants
  private val maxBound = 200
  private val gradientMetric: Metric = () => nbrRange()
  private val crfRisingSpeed = 20
  private lazy val radius = node.get[java.lang.Double]("range")
  /// Application domain definition
  private lazy val leftSrc: Int =
    node.get[Integer]("left_source") // ID of the source at the left of the env (the stable one)
  private lazy val rightSrc: Int =
    node.get[Integer]("right_source") // ID of the source at the right of the env (the unstable one)
  private lazy val rightSrcStop: Int =
    node.get[Integer]("stop_right_source") // time at which the source at the right of the env stops being a source
  def source: Boolean =
    if (mid() == leftSrc || (mid() == rightSrc && passedTime() < rightSrcStop)) true else false
  /// Learning definition
  private lazy val shouldLearn: Boolean = learnCondition && !source
  // Aggregate program
  override def aggregateProgram(): Unit = {
    ///// BASELINE
    val classic = classicGradient(source)
    ///// OPTIMAL REFERENCE
    val whenRightIsDown =
      classicGradient(mid() == leftSrc) // optimal gradient when RIGHT_SRC stops being a source
    val referenceGradient = if (passedTime() >= rightSrcStop) whenRightIsDown else classic
    node.put("reference", referenceGradient) // because it will be used by the other algorithms
    //// STATE OF THE ART
    val crf = crfGradient(crfRisingSpeed)(source = source, gradientMetric)
    //// DATA STORAGE
    val outputRL = rlGradient(shouldLearn)
    /// OUTPUT
    node.put("output_classicHopCount", classic)
    node.put("output_reference", referenceGradient)
    node.put("output_crf", crf)
    /// ERROR
    node.put("err_classicHopCount", outputEvaluation(referenceGradient, classic))
    node.put(s"err_crf", outputEvaluation(referenceGradient, crf))
    /// RL DATA
    node.put(s"output_temporalRL", outputRL)
    node.put(s"err_temporalRL", outputEvaluation(referenceGradient, outputRL))
  }

  protected def outputEvaluation(ref: Double, value: Double): Double = {
    val result = (ref - value).abs
    if (ref.isInfinite && value.isInfinite) {
      0.0
    } else if (result > maxBound) { alchemistEnvironment.getNodes.size() }
    else { result }
  }

  def rlGradient(learn: Boolean): Double = {
    rep(Double.PositiveInfinity) { case (g) =>
      mux(source)(0.0) {
        val currentState = state(g)
        val (index, action) = DeepReinforcementLearning.pass(currentState, learn)
        val output = minHoodPlus(nbr(g) + nbrRange())
        val result = action match {
          case ConsiderNeighbourhood => output
          case IncreaseWith(v)       => g + v * (deltaTime().toMillis / 1000.0)
        }
        val reward = rewardSignal(result)
        branch(learn && !source) {
          val trajectory =
            node.getOrElse[Queue[(Seq[Double], Int, Double)]]("trajectory", Queue.empty[(List[Double], Int, Double)])
          node.put("trajectory", trajectory :+ (currentState, index, reward))
        } {}
        result
      }
    }
  }

  def state(output: Double): Seq[Double] = {
    val all = (excludingSelf
      .reifyField(nbr(output))
      .values
      ++ List(0.0, 0.0, 0.0, 0.0)).take(4)

    val min = (all.min - output) / radius
    val max = (all.max - output) / radius
    Seq(min, max).map {
      case a if a.isFinite => a
      case _               => 0.0
    }
  }

  protected def rewardSignal(output: Double): Double = {
    if ((node.get[Double]("reference") - output) ~= 0) {
      0
    } else { -1 }
  }

  override lazy val endHandler: EndHandler[_] = {
    import DeepReinforcementLearning._
    val storeMonitor = new EndHandler[Any](
      sharedLogic = () => {},
      leaderLogic = () => {
        if (shouldLearn) {
          val nodes = alchemistEnvironment.getNodes.asScala.toList
          val trajectories =
            nodes.map(new SimpleNodeManager(_)).map(_.get[Queue[(List[Double], Int, Double)]]("trajectory"))
          val dropped = trajectories.filter(_ != null).map(_.take(80)).filter(_.size >= 80).flatten
          val states = dropped.map(data => np.array(data._1.toPythonCopy)).toPythonCopy
          val actions = dropped.map(_._2).toPythonCopy
          val rewards = dropped.map(_._3).toPythonCopy

          val terminations = List.tabulate(dropped.size)(_ => List.fill(79)(0) ::: 1 :: Nil).flatten.toPythonCopy
          val dataset =
            d3rlpy.dataset.MDPDataset(np.array(states), np.array(actions), np.array(rewards), np.array(terminations))

          val fitted = dqn.fit(dataset, n_epochs = 10)
          states.del()
          actions.del()
          rewards.del()
          terminations.del()
        }
      },
      id = mid()
    )

    alchemistEnvironment.getSimulation.addOutputMonitor(storeMonitor)
    storeMonitor
  }
}
