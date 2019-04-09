package fr.ramiro.scala.fuzzylogic

import java.io.File

import org.jfree.chart.{ChartFactory, ChartPanel, ChartUtilities}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.ui.ApplicationFrame

object Charts {

  def frameFuzzySet(fuzzySet: FuzzySet, width: Int = 640, height: Int = 480) = new ApplicationFrame(fuzzySet.name) {
    setContentPane(new ChartPanel(chartFuzzySet(fuzzySet)))
    setSize(width, height)
  }

  def imageFuzzySet(fuzzySet: FuzzySet, width: Int = 640, height: Int = 480) = {
    ChartUtilities.saveChartAsJPEG(new File(s"${fuzzySet.name}.jpeg"), chartFuzzySet(fuzzySet), width, height)
  }

  def chartFuzzySet(fuzzySet: FuzzySet) =
    ChartFactory.createXYLineChart(fuzzySet.name,
                                   "y",
                                   "x",
                                   dataFromFuzzySet(fuzzySet),
                                   PlotOrientation.VERTICAL,
                                   true,
                                   true,
                                   false)

  def dataFromFuzzySet(fuzzySet: FuzzySet) = {
    val dataset = new XYSeriesCollection
    for {
      (value, ms) <- fuzzySet.memberships
    } {
      val serie = new XYSeries(value)
      for {
        x <- Range.BigDecimal(fuzzySet.minValue, fuzzySet.maxValue, fuzzySet.deltaX)
      } {
        serie.add(x, ms(x.doubleValue()))
      }
      dataset.addSeries(serie)
    }
    dataset
  }
}
