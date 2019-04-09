package fr.ramiro.scala.fuzzylogic

package object defuzzifiers {
  def areaCentroid(defuzzify: Defuzzify)(degreesMap: Degrees): Double = {
    Range.BigDecimal(defuzzify.minValue, defuzzify.maxValue, defuzzify.deltaX).foldLeft((0d, 0d)) {
      case ((sumxy, sumy), x) =>
        degreesMap.foldLeft((sumxy, sumy)) {
          case ((sxy, sy), (value, degrees)) =>
            val rootSumSquare = Math.sqrt(degrees.map(v => v * v).sum)
            val y             = Math.min(rootSumSquare, value.membership.apply(x.doubleValue()))

            (sxy + (x.toDouble * y), sy + y)
        }
    } match {
      case (sumxy, sumy) if sumy == 0d && sumxy == 0d => 1 //TODO confirm
      case (_, sumy) if sumy == 0d                    => defuzzify.defaultValue
      case (sumxy, sumy)                              => sumxy / sumy
    }
  }
}
