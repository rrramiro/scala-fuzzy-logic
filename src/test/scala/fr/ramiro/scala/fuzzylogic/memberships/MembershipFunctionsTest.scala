package fr.ramiro.scala.fuzzylogic.memberships

import org.scalactic.Equality
import org.scalatest.FunSuite

import scala.io.Source

class MembershipFunctionsTest extends FunSuite {
  def tolerantFuzzyEvaluationTypeEquality(tolerance: Double): Equality[Double] = {
    if (tolerance <= (0: BigDecimal))
      throw new IllegalArgumentException(tolerance.toString + " passed to tolerantFuzzyEvaluationTypeEquality was zero or negative. Must be a positive non-zero number.")
    new Equality[Double] {
      def areEqual(a: Double, b: Any): Boolean = {
        b match {
          case bFuzzyEvaluationType: Number =>
            (a <= tolerance + bFuzzyEvaluationType.doubleValue()) && (a >= (-tolerance) + bFuzzyEvaluationType.doubleValue())
          case _ => false
        }
      }
      override def toString: String = s"TolerantFuzzyEvaluationTypeEquality($tolerance)"
    }
  }

  private implicit val fuzzyEvaluationEquality: Equality[Double] = tolerantFuzzyEvaluationTypeEquality(1e-2d)

  test("Cosine") {
    val poor = consine(1, 0)
    val good = consine(4, 4)
    val excellent = consine(2, 8)
    assertPoorGoodExcellent("cosine.txt", poor, good, excellent)
  }

  test("dsigm") {
    val poor = differenceSigmoidal(1, 1, 1, 3)
    val good = differenceSigmoidal(9, 4, 9, 6)
    val excellent = differenceSigmoidal(1, 8, 5, 9)
    assertPoorGoodExcellent("dsigm.txt", poor, good, excellent)
  }

  test("Gauss") {
    val poor = gauss(2, 2)
    val good = gauss(5, 3)
    val excellent = gauss(8, 4)
    assertPoorGoodExcellent("gauss.txt", poor, good, excellent)
  }

  test("generalizedBell") {
    val poor = generalizedBell(2, 4, 2)
    val good = generalizedBell(4, 4, 5)
    val excellent = generalizedBell(2, 1, 8)
    assertPoorGoodExcellent("gbell.txt", poor, good, excellent)
  }

  test("Piece-wise Linear") {
    val poor = piecewiseLinear((0, 1), (2, 1), (4, 0))
    val good = piecewiseLinear((1, 0), (2, 0.5), (3, 0.7), (4, 1), (4.5, 1), (5, 0.6), (6, 0.3), (7, 0.3), (8, 0.8), (9, 0.8), (10, 0))
    val excellent = piecewiseLinear((6, 0), (9, 1), (10, 1))
    assertPoorGoodExcellent("piecewise_linear.txt", poor, good, excellent)
  }

  test("sigmoid") {
    val poor = sigmoidal(-4, 3)
    val good = sigmoidal(2, 5)
    val excellent = sigmoidal(9, 8)
    assertPoorGoodExcellent("sigmoid.txt", poor, good, excellent)
  }

  test("singletons") {
    val poor = singleton(2)
    val good = singleton(5)
    val excellent = singleton(8)
    assertPoorGoodExcellent("singletons.txt", poor, good, excellent)
  }

  test("trape") {
    val poor = trapezoidal(0, 2, 3, 4)
    val good = trapezoidal(3, 4, 5, 6)
    val excellent = trapezoidal(5, 6, 8, 10)
    assertPoorGoodExcellent("trape.txt", poor, good, excellent)
  }

  test("triang") {
    val poor = triangular(0, 2.5, 5)
    val good = triangular(2.5, 5, 7.5)
    val excellent = triangular(5, 7.5, 10)
    assertPoorGoodExcellent("triang.txt", poor, good, excellent)
  }

  private def assertPoorGoodExcellent(fileName: String, poor: Membership, good: Membership, excellent: Membership): Unit = {
    Source.fromResource(fileName).getLines().map { _.split("\t").toList.collect { case n if n.matches("\\d+") => n.toDouble / 100.0d } }.filter(_.nonEmpty).foreach {
      case input :: poorValue :: goodValue :: excellentValue :: Nil =>
        assert(poor(input) === poorValue)
        assert(good(input) === goodValue)
        assert(excellent(input) === excellentValue)
      case _ => fail("fail assert")
    }
  }
}
