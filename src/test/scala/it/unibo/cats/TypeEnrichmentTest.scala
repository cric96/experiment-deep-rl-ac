package it.unibo.cats

import cats.data.NonEmptyList
import it.unibo.cats.TypeEnrichment._
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TypeEnrichmentTest extends AnyFlatSpec with should.Matchers {

  "NonEmptyList" should "compute the maximum value" in {
    NonEmptyList.of((1, 0), (2,0), (3,0)).maxBy(_._1) shouldBe (3, 0)
  }
}
