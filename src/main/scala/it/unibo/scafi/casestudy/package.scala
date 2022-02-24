package it.unibo.scafi

package object casestudy {
  implicit class DoubleWithAlmostEquals(val d: Double) {
    def default = 0.00001
    def ~=(d2: Double) = (d - d2).abs < default
  }
}
