package fr.ramiro.scala.fuzzylogic

import org.jzy3d.chart.controllers.mouse.camera.AWTCameraMouseController
import org.jzy3d.chart.factories.AWTChartComponentFactory
import org.jzy3d.chart.factories.IChartComponentFactory.Toolkit
import org.jzy3d.colors._
import org.jzy3d.colors.colormaps._
import org.jzy3d.maths.Coord3d
import org.jzy3d.plot3d.builder._
import org.jzy3d.plot3d.rendering.canvas.Quality

import scala.collection.JavaConverters._

object Charts3D {
  def chart3dFuzzySet(rules: Seq[Rule], xFuzzySet: Fuzzify, yFuzzySet: Fuzzify, zFuzzySet: Defuzzify) = {
    val coords = for {
      x <- Range.BigDecimal(xFuzzySet.minValue, xFuzzySet.maxValue, xFuzzySet.deltaX)
      y <- Range.BigDecimal(yFuzzySet.minValue, yFuzzySet.maxValue, yFuzzySet.deltaX)

    } yield {
      val z = zFuzzySet.evaluate(rules,
                                 Map(
                                   xFuzzySet -> x.doubleValue(),
                                   yFuzzySet -> y.doubleValue()
                                 ))
      new Coord3d(x.doubleValue(), y.doubleValue(), z)
    }

    val surface = Builder.buildDelaunay(coords.toList.asJava)
    surface.setColorMapper(new ColorMapper(new ColorMapRainbow, surface.getBounds.getZRange))
    surface.setFaceDisplayed(true)
    surface.setWireframeDisplayed(false)
    surface.setWireframeColor(Color.BLACK)

    val componentFactory = new AWTChartComponentFactory
    val chart            = componentFactory.newChart(Quality.Intermediate, Toolkit.awt)
    val mouse            = chart.addMouseCameraController().asInstanceOf[AWTCameraMouseController]
    chart.setAnimated(false)
    mouse.setUpdateViewDefault(!chart.getQuality.isAnimated)
    chart.add(surface)
    componentFactory.newFrame(chart)
  }
}
