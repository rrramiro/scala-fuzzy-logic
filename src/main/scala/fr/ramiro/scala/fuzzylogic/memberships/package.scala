package fr.ramiro.scala.fuzzylogic

package object memberships {
  trait Membership extends (Double => Double)

  def triangular(min: Double, mid: Double, max: Double): Membership = {
    case x: Double if x <= min || x >= max   => 0
    case x: Double if x == mid               => 1
    case x: Double if (x > min) && (x < mid) => (x - min) / (mid - min)
    case x: Double                           => (max - x) / (max - mid)
  }

  def trapezoidal(min: Double, midLow: Double, midHigh: Double, max: Double): Membership = {
    case x: Double if x <= min || x >= max        => 0
    case x: Double if x >= midLow && x <= midHigh => 1
    case x: Double if (x > min) && (x < midLow)   => (x - min) / (midLow - min)
    case x: Double                                => (max - x) / (max - midHigh)
  }

  def reverseGrade(lowerValue: Double, upperValue: Double): Membership = {
    case x: Double if x <= lowerValue => 1
    case x: Double if x < upperValue  => (upperValue - x) / (upperValue - lowerValue)
    case _                            => 0
  }

  def grade(lowerValue: Double, upperValue: Double): Membership = {
    case x: Double if x <= lowerValue => 0
    case x: Double if x < upperValue  => (x - lowerValue) / (upperValue - lowerValue)
    case _                            => 1
  }

  def consine(a: Double, b: Double): Membership = {
    case x: Double if (-a <= (x - b)) && ((x - b) <= a) =>
      (Math.cos((x - b) * Math.PI / a) + 1.0d) / 2.0d
    case _ =>
      0.0d
  }

  def gauss(mean: Double, stdev: Double): Membership = { x: Double =>
    Math.exp(-((x - mean) * (x - mean) / (2 * stdev * stdev)))
  }

  def gauss2(meanl: Double, stdevl: Double, meanr: Double, stdevr: Double): Membership = {
    case x: Double if x < meanl =>
      Math.exp(-((x - meanl) * (x - meanl) / (2.0d * stdevl * stdevl)))
    case in: Double if in > meanr =>
      Math.exp(-((in - meanr) * (in - meanr) / (2.0d * stdevr * stdevr)))
    case _ =>
      1.0d
  }

  def generalizedBell(a: Double, b: Double, mean: Double): Membership = { x: Double =>
    1.0d / (1.0d + Math.pow(Math.abs((x - mean) / a), 2.0d * b))
  }

  def sigmoidal(gain: Double, center: Double): Membership = { x: Double =>
    1.0d / (1.0d + Math.exp(-(gain * (x - center))))
  }

  def differenceSigmoidal(standardDeviation1: Double,
                          mean1: Double,
                          standardDeviation2: Double,
                          mean2: Double): Membership = { x: Double =>
    val sigm1 = 1.0d / (1.0d + Math.exp(-(standardDeviation1 * (x - mean1))))
    val sigm2 = 1.0d / (1.0d + Math.exp(-(standardDeviation2 * (x - mean2))))
    val diff  = sigm1 - sigm2
    Math.max(diff, 0.0d)
  }

  def singleton(value: Double): Membership = {
    case x: Double if x == value => 1.0d
    case _                       => 0.0
  }

  def piecewiseLinear(points: (Double, Double)*): Membership = {
    def linearRec(f: PartialFunction[Double, Double],
                  points: List[(Double, Double)]): PartialFunction[Double, Double] = points match {
      case Nil         => f
      case head :: Nil => f orElse lastFunc(head)
      case pointA :: pointB :: tail =>
        linearRec(f orElse line(pointA, pointB), pointB :: tail)
    }

    def headFunc(pointA: (Double, Double)): PartialFunction[Double, Double] = {
      case x: Double if x <= pointA._1 => pointA._2
    }

    def lastFunc(pointB: (Double, Double)): PartialFunction[Double, Double] = {
      case x: Double if x >= pointB._1 => pointB._2
    }

    def line(pointA: (Double, Double), pointB: (Double, Double)): PartialFunction[Double, Double] = {
      case x: Double if x >= pointA._1 && x <= pointB._1 =>
        val ((x1, y1), (x2, y2)) = (pointA, pointB)
        y1 + (y2 - y1) * ((x - x1) / (x2 - x1))
    }

    if (points.isEmpty) { _: Double =>
      0d
    } else {
      linearRec(headFunc(points.head), points.toList)(_)
    }
  }
}
