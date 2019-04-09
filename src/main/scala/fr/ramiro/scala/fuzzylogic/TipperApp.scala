package fr.ramiro.scala.fuzzylogic

import fr.ramiro.scala.fuzzylogic.memberships._

object TipperApp extends App {

  object food extends Fuzzify("food", 0, 9, 0.1) {
    object rancid    extends Term("rancid", piecewiseLinear((0, 1), (1, 1), (3, 0)))
    object delicious extends Term("delicious", piecewiseLinear((7, 0), (9, 1)))
  }

  object service extends Fuzzify("service", 0, 9, 0.1) {
    object poor      extends Term("poor", piecewiseLinear((0, 1), (4, 0)))
    object good      extends Term("good", piecewiseLinear((1, 0), (4, 1), (6, 1), (9, 0)))
    object excellent extends Term("excellent", piecewiseLinear((6, 0), (9, 1)))
  }

  object tip extends Defuzzify("tip", 0, 30, 0.1) {
    object cheap    extends Term("cheap", piecewiseLinear((0, 0), (5, 1), (10, 0)))
    object average  extends Term("average", piecewiseLinear((10, 0), (15, 1), (20, 0)))
    object generous extends Term("generous", piecewiseLinear((20, 0), (25, 1), (30, 0)))
  }

  /*
    IF((service IS poor) OR (food IS rancid)) THEN (tip IS cheap),
    IF(service IS good) THEN (tip IS average),
    IF((service IS excellent) AND (food IS delicious)) THEN (tip IS generous)
   */

  val rules = Seq(
    Rule("Rule 1", service.poor or food.rancid, tip.cheap),
    Rule("Rule 2", service.good, tip.average),
    Rule("Rule 3", service.excellent and food.delicious, tip.generous)
  )

  val frames = Seq(
    Charts.frameFuzzySet(food),
    Charts.frameFuzzySet(service),
    Charts.frameFuzzySet(tip)
  )

  frames.foreach { frame =>
    frame.setSize(560, 367)
    frame.setVisible(true)
  }

  Charts3D.chart3dFuzzySet(rules, food, service, tip)

  val param = Map(
    service -> 3d,
    food    -> 7d
  )

  val result = tip.evaluate(rules, param)
  println(result)
  implicit class DoubleWrapper(left: Double) {
    def ~=(right: Double, precision: Double): Boolean = (left - right).abs < precision
  }
  assert(result ~= (11.701603788948043, 0.0003))

}
