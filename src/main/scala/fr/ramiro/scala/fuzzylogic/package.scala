package fr.ramiro.scala

import fr.ramiro.scala.fuzzylogic.memberships.Membership
import fr.ramiro.scala.fuzzylogic.defuzzifiers._

import scala.collection.mutable

package object fuzzylogic {

  type EvalParams = Map[Fuzzify, Double]
  type Degrees    = Map[Consequent, Seq[Double]]

  //TODO define "and" and "or" operation in parameter

  def not(ruleExpression: RuleExpression): RuleExpression = 1 - ruleExpression(_)

  trait RuleExpression extends (EvalParams => Double) {
    def or(that: RuleExpression): RuleExpression = { param: EvalParams =>
      Math.max(this(param), that(param))
    }
    def and(that: RuleExpression): RuleExpression = { param: EvalParams =>
      Math.min(this(param), that(param))
    }
  }

  trait Consequent {
    def variable: Defuzzify
    def membership: Membership
  }

  trait FuzzySet {
    def name: String
    def minValue: Double
    def maxValue: Double
    def deltaX: Double
    def memberships: mutable.HashMap[String, Membership]
  }

  abstract case class Fuzzify(
      name: String,
      minValue: Double = 0d,
      maxValue: Double = 1d,
      deltaX: Double   = 0.0001
  ) extends FuzzySet {
    val memberships = new mutable.HashMap[String, Membership]()

    abstract class Term(variableValue: String, membership: Membership) extends RuleExpression {
      memberships.put(variableValue, membership)
      def apply(param: EvalParams): Double = membership(param(Fuzzify.this))
    }
  }

  abstract case class Defuzzify(
      name: String,
      minValue: Double     = 0d,
      maxValue: Double     = 1d,
      deltaX: Double       = 0.0001,
      defaultValue: Double = 0d //TODO Default value if no rule activates defuzzifier
      //TODO activation method, accumulation method and defuzzification method
  ) extends FuzzySet {
    val memberships = new mutable.HashMap[String, Membership]()

    //TODO weight
    abstract case class Term(variableValue: String, membership: Membership, weight: Double = 1.0) extends Consequent {
      memberships.put(variableValue, membership)
      val variable: Defuzzify = Defuzzify.this
    }

    def evaluate(rules: Seq[Rule], param: EvalParams): Double = {
      areaCentroid(this) {
        rules
          .filter(_.consequent.variable eq this)
          .foldLeft(
            Map.empty[Consequent, Seq[Double]].withDefaultValue(Seq.empty)
          ) {
            case (degreesMap, rule) =>
              degreesMap + (rule.consequent -> (degreesMap(rule.consequent) :+ rule.antecedents(param)))
          }
      }
    }
  }

  case class Rule(name: String, antecedents: RuleExpression, consequent: Consequent)
}
