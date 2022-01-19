package it.unibo.scafi.casestudy
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.implicitConversions

@SuppressWarnings(
  Array("org.wartremover.warts.DefaultArguments", "org.wartremover.warts.TraversableOps")
) // because of should be in scafi lib
trait FixedGradients extends GenericUtils with StateManagement {
  self: FieldCalculusSyntax with StandardSensors with BlockG =>
  import Builtins._

  val DEFAULT_CRF_RAISING_SPEED: Double = 5.0
  case class FixedGradient(
      algorithm: (Boolean, () => Double) => Double,
      source: Boolean = false,
      metric: Metric = nbrRange
  ) {
    def from(s: Boolean): FixedGradient = this.copy(source = s)
    def withMetric(m: Metric): FixedGradient = this.copy(metric = m)
    def run(): Double = algorithm(source, metric)
  }
  implicit def tupleBounded4[T1, T2, T3, T4](implicit
      of1: Bounded[T1],
      of2: Bounded[T2],
      of3: Bounded[T3],
      of4: Bounded[T4]
  ): Bounded[(T1, T2, T3, T4)] =
    new Bounded[(T1, T2, T3, T4)] {
      def top: (T1, T2, T3, T4) = (of1.top, of2.top, of3.top, of4.top)
      def bottom: (T1, T2, T3, T4) = (of1.bottom, of2.bottom, of3.top, of4.top)
      override def compare(a: (T1, T2, T3, T4), b: (T1, T2, T3, T4)): Int = {
        List(of1.compare(a._1, b._1), of2.compare(a._2, b._2), of3.compare(a._3, b._3), of4.compare(a._4, b._4))
          .filter(_ != 0)
          .lift(0)
          .getOrElse(0)
      }
    }

  val ClassicHopGradient: FixedGradient = FixedGradient((src, _) => hopGradient(src), source = false, () => 1)
  def BisGradient(
      commRadius: Double = 0.2,
      lagMetric: => Double = nbrLag().toUnit(TimeUnit.MILLISECONDS)
  ): FixedGradient =
    FixedGradient(bisGradient(commRadius, lagMetric), source = false, nbrRange)
  def CrfGradient(
      raisingSpeed: Double = 5,
      lagMetric: => Double = nbrLag().toUnit(TimeUnit.MILLISECONDS)
  ): FixedGradient =
    FixedGradient(crfGradient(raisingSpeed), source = false, nbrRange)
  def FlexGradient(epsilon: Double = 0.5, delta: Double = 1.0, communicationRadius: Double = 1.0): FixedGradient =
    FixedGradient(flexGradient(epsilon, delta, communicationRadius), source = false, nbrRange)
  def SvdGradient(lagMetric: => Double = nbrLag().toUnit(TimeUnit.MILLISECONDS)): FixedGradient =
    FixedGradient(svdGradient(lagMetric), source = false, nbrRange)

  def hopGradient(source: Boolean): Double =
    rep(Double.PositiveInfinity) { hops =>
      mux(source)(0.0)(1 + minHood(nbr(hops)))
    }

  def bisGradient(
      commRadius: Double = 0.2,
      lagMetric: => Double = nbrLag().toMillis.toDouble
  )(source: Boolean, metric: Metric = nbrRange): Double = {
    val avgFireInterval = meanCounter(deltaTime().toUnit(TimeUnit.MILLISECONDS), 1.second.toMicros)
    val speed = 1 //(1.0 / avgFireInterval)
    rep((Double.PositiveInfinity, Double.PositiveInfinity)) { case (spatialDist: Double, tempDist: Double) =>
      mux(source) {
        (0.0, 0.0)
      } {
        val result = minHoodPlus {
          val newEstimate = Math.max(nbr(spatialDist) + metric(), speed * nbr(tempDist) - commRadius)
          (newEstimate, nbr(tempDist) + lagMetric / 1000.0)
        }
        result
      }
    }._1
  }

  case class RaisingDist(dist: Double, raising: Boolean) {
    def +(delta: Double): RaisingDist =
      RaisingDist(dist + delta, raising)
    def min(o: RaisingDist): RaisingDist = {
      if (raising == o.raising) {
        if (dist < o.dist) this else o
      } else {
        if (raising) o else this
      }
    }
  }

  def crfGradient(raisingSpeed: Double = DEFAULT_CRF_RAISING_SPEED, lagMetric: => Double = nbrLag().toMillis.toDouble)(
      source: Boolean,
      metric: Metric = nbrRange
  ): Double =
    rep((Double.PositiveInfinity, 0.0)) { case (g, speed) =>
      mux(source)((0.0, 0.0)) {
        implicit def durationToDouble(fd: FiniteDuration): Double = fd.toMillis.toDouble / 1000.0
        final case class Constraint(nbr: ID, gradient: Double, nbrDistance: Double)

        val constraints = foldhoodPlus[List[Constraint]](List.empty)(_ ++ _) {
          val (nbrg, d) = (nbr(g), metric())
          mux(nbrg + d + speed * lagMetric / 1000.0 <= g)(List(Constraint(nbr(mid()), nbrg, d)))(List())
        }

        if (constraints.isEmpty) {
          (g + raisingSpeed * deltaTime(), raisingSpeed)
        } else {
          (constraints.map(c => c.gradient + c.nbrDistance).min, 0.0)
        }
      }
    }._1

  /** Idea: a device should change its estimate only for significant errors. Useful when devices far from the source
    * need only coarse estimates. Flex gradient provides tunable trade-off between precision and communication cost.
    *
    * @param source
    *   Source fields of devices from which the gradient is calculated
    * @param epsilon
    *   Parameter expressing tolerance wrt changes
    * @param delta
    *   Distortion into the distance measure, such that neighbor distance is never considered to be less than delta *
    *   communicationRadius.
    * @param communicationRadius
    * @return
    */
  def flexGradient(epsilon: Double = 0.5, delta: Double = 1.0, communicationRadius: Double = 1.0)(
      source: Boolean,
      metric: Metric = nbrRange
  ): Double =
    rep(Double.PositiveInfinity) { g =>
      def distance = Math.max(nbrRange(), delta * communicationRadius)
      val maxLocalSlope: (Double, ID, Double, Double) =
        maxHood {
          ((g - nbr(g)) / distance, nbr(mid()), nbr(g), metric())
        }
      val constraint = minHoodPlus((nbr(g) + distance))

      mux(source)(0.0) {
        if (Math.max(communicationRadius, 2 * constraint) < g) {
          constraint
        } else if (maxLocalSlope._1 > 1 + epsilon) {
          maxLocalSlope._3 + (1 + epsilon) * Math.max(delta * communicationRadius, maxLocalSlope._4)
        } else if (maxLocalSlope._1 < 1 - epsilon) {
          maxLocalSlope._3 + (1 - epsilon) * Math.max(delta * communicationRadius, maxLocalSlope._4)
        } else {
          g
        }
      }
    }

  def svdGradient(lagMetric: => Double = nbrLag().toMillis.toDouble)(
      source: Boolean,
      metric: Metric = nbrRange
  ): Double = {

    /** At the heart of SVD algorithm. This function is responsible to kick-start the reconfiguration process.
      *
      * @param time
      * @return
      */
    def detect(time: Double, threshold: Double = 0.0001): Boolean = {
      // Let's keep track into repCount of how much time is elapsed since the first time
      // the current info (originated from the source in time 'time') reached the current device
      val repCount = rep(0.0) { old =>
        if (Math.abs(time - delay(time)) < threshold) {
          old + deltaTime().toMillis
        } else {
          0.0
        }
      }

      val obsolete = repCount > rep[(Double, Double, Double)](2, 8, 16) { case (avg, sqa, _) =>
        // Estimate of the average peak value for repCount, obtained by exponentially filtering
        // with a factor 0.1 the peak values of repCount
        val newAvg = 0.9 * avg + 0.1 * delay(repCount)
        // Estimate of the average square of repCount peak values
        val newSqa = 0.9 * sqa + 0.1 * Math.pow(delay(repCount), 2)
        // Standard deviation
        val stdev = Math.sqrt(newSqa - Math.pow(newAvg, 2))
        // New bound
        val newBound = newAvg + 7 * stdev
        (newAvg, newSqa, newBound)
      }._3

      obsolete
    }

    val defaultDist = if (source) 0.0 else Double.PositiveInfinity
    val loc = (defaultDist, defaultDist, mid(), false)
    // REP tuple: (spatial distance estimate, temporal distance estimate, source ID, obsolete value detected flag)
    rep[(Double, Double, ID, Boolean)](loc) { case old @ (spaceDistEst, timeDistEst, sourceId, isObsolete) =>
      // (1) Let's calculate new values for spaceDistEst and sourceId
      val (newSpaceDistEst: Double, newSourceId: ID) =
        minHood {
          mux(nbr(isObsolete) && excludingSelf.anyHood(!nbr(isObsolete))) { // let's discard neighbours where 'obsolete' flag is true
            // (unless 'obsolete' flag is true for all the neighbours)
            (defaultDist, mid())
          } {
            // if info is not obsolete OR all nbrs have obsolete info
            // let's use classic gradient calculation
            (nbr(spaceDistEst) + metric(), nbr(sourceId))
          }
        }

      // (2) The most recent timeDistEst for the newSourceId is retrieved
      // by minimising nbrs' values for timeDistEst + their relative time distance
      // (we only consider neighbours that have same value for 'sourceId')
      val newTimeDistEst = minHood {
        mux(nbr(sourceId) != newSourceId) {
          // let's discard neighbours with a sourceId different than newSourceId
          defaultDist
        } {
          nbr(timeDistEst) + lagMetric
        }
      }

      // (3) Let's compute if the newly produced info is to be considered obsolete
      val loop = newSourceId == mid() && newSpaceDistEst < defaultDist
      val newObsolete =
        detect(timestamp() - newTimeDistEst) || // (i) if the time when currently used info started
          //     from sourceId is too old to be reliable
          loop || // or, (ii) if the device's value happens to be calculated from itself,
          excludingSelf.anyHood { // or, (iii) if any (not temporally farther) nbr with same sourceId  than
            //           the device's one has already been claimed obsolete
            nbr(isObsolete) && nbr(sourceId) == newSourceId && nbr {
              timeDistEst
            } + lagMetric < newTimeDistEst + 0.0001
          }

      //List[(Double,Double,ID,Boolean)]((newSpaceDistEst, newTimeDistEst, newSourceId, newObsolete), loc).min
      if (newSpaceDistEst >= loc._1) {
        if (newTimeDistEst >= loc._2) {
          if (newObsolete >= loc._4) {
            loc
          } else {
            (newSpaceDistEst, newTimeDistEst, newSourceId, newObsolete)
          }
        } else {
          (newSpaceDistEst, newTimeDistEst, newSourceId, newObsolete)
        }
      } else {
        (newSpaceDistEst, newTimeDistEst, newSourceId, newObsolete)
      }
    }._1 // Selects estimated distance
  }

}
