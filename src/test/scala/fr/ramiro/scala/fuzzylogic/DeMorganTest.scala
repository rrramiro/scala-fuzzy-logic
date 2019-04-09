package fr.ramiro.scala.fuzzylogic
import fr.ramiro.scala.fuzzylogic.memberships._
import org.scalatest.FunSuite

class DeMorganTest extends FunSuite {
  object x1 extends Fuzzify("x1", 0, 1) {
    object value extends Term("x1value", piecewiseLinear((0, 1), (0, 1)))
  }

  object x2 extends Fuzzify("x2", 0, 1) {
    object value extends Term("x2value", piecewiseLinear((0, 1), (0, 1)))
  }

  object y extends Defuzzify("y", 0, 1) {
    object value extends Term("y", piecewiseLinear((0, 1), (0, 1)))
  }

  val rules = Seq(
    Rule("Rule 1 ", not(x1.value or x2.value) , y.value),
    Rule("Rule 2 ", not(x1.value) and not(x2.value), y.value),
    Rule("Rule 3 ", not(x1.value and x2.value), y.value),
    Rule("Rule 4 ", not(x1.value) or not(x2.value), y.value)
  )

  //TODO
}
