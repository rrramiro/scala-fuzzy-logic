package fr.ramiro.scala.fuzzylogic

import fr.ramiro.scala.fuzzylogic.memberships._
import org.slf4j.LoggerFactory

object TemperatureApp extends App {
  private val logger = LoggerFactory.getLogger(this.getClass)

  object output extends Defuzzify("Output", -100, 100, 1) {
    object cooling extends Term("Cooling", reverseGrade(-50, 0))
    object zero    extends Term("Zero", triangular(-50, 0, 50))
    object heating extends Term("Heating", grade(0, 50))
  }

  object tempError extends Fuzzify("Temperature Error", -4, 4, 0.05) {
    object negative extends Term("Negative", reverseGrade(-2, 0))
    object zero     extends Term("Zero", triangular(-2, 0, 2))
    object positive extends Term("Positive", grade(0, 2))
  }

  object tempErrorDot extends Fuzzify("Temperature Error Dot", -10, 10, 0.1) {
    object negative extends Term("Negative", reverseGrade(-5, 0))
    object zero     extends Term("Zero", triangular(-5, 0, 5))
    object positive extends Term("Positive", grade(0, 5))
  }

  /*
  1. If (e < 0) AND (er < 0) then Cool 0.5 & 0.0 = 0.0
  2. If (e = 0) AND (er < 0) then Heat 0.5 & 0.0 = 0.0
  3. If (e > 0) AND (er < 0) then Heat 0.0 & 0.0 = 0.0
  4. If (e < 0) AND (er = 0) then Cool 0.5 & 0.5 = 0.5
  5. If (e = 0) AND (er = 0) then No_Chng 0.5 & 0.5 = 0.5
  6. If (e > 0) AND (er = 0) then Heat 0.0 & 0.5 = 0.0
  7. If (e < 0) AND (er > 0) then Cool 0.5 & 0.5 = 0.5
  8. If (e = 0) AND (er > 0) then Cool 0.5 & 0.5 = 0.5
  9. If (e > 0) AND (er > 0) then Heat 0.0 & 0.5 = 0.0
   */

  val rie =
    Seq(
      Rule("Rule 1", tempError.negative and tempErrorDot.negative, output.cooling),
      Rule("Rule 2", tempError.zero and tempErrorDot.negative, output.heating),
      Rule("Rule 3", tempError.positive and tempErrorDot.negative, output.heating),
      Rule("Rule 4", tempError.negative and tempErrorDot.zero, output.cooling),
      Rule("Rule 5", tempError.zero and tempErrorDot.zero, output.zero),
      Rule("Rule 6", tempError.positive and tempErrorDot.zero, output.heating),
      Rule("Rule 8", tempError.zero and tempErrorDot.positive, output.cooling),
      Rule("Rule 9", tempError.positive and tempErrorDot.positive, output.heating)
    )

  /*
  val param = Map(
    tempError -> -1.0d,
    tempErrorDot -> 2.5d
  )
   */
  val param = Map(
    tempError    -> -3.8d,
    tempErrorDot -> 5.0d
  )

  val frames = Seq(
    Charts.frameFuzzySet(tempError),
    Charts.frameFuzzySet(tempErrorDot),
    Charts.frameFuzzySet(output)
  )

  frames.foreach { frame =>
    frame.setVisible(true)
  }

  Charts3D.chart3dFuzzySet(rie, tempError, tempErrorDot, output)

  logger.info("For Temperature Error: {}", param(tempError))
  logger.info("Negative: " + tempError.memberships("Negative").apply(-1.0))
  logger.info("Zero: " + tempError.memberships("Zero").apply(-1.0))
  logger.info("Positive: " + tempError.memberships("Positive").apply(-1.0))

  logger.info("For Temperature Error Dot: {}", param(tempErrorDot))
  logger.info("Negative: " + tempErrorDot.memberships("Negative").apply(2.5))
  logger.info("Zero: " + tempErrorDot.memberships("Zero").apply(2.5))
  logger.info("Positive: " + tempErrorDot.memberships("Positive").apply(2.5))

  val result = output.evaluate(rie, param)
  //-35.639629442712426
  logger.info("output: {} = {}", output.name, result)

}
