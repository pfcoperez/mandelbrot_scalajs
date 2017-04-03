package org.pfcoperez.mandelbrotjs

import scala.scalajs.js.JSApp

import org.scalajs.dom
import dom.document
import dom.raw.HTMLCanvasElement

object Geom2D {

  trait Vector[T] { val x: T; val y: T }

  case class Pixel(x: Long, y: Long) extends Vector[Long]
  case class Point(x: Double, y: Double) extends Vector[Double]

  trait Frame[T] {
    val upperLeft: Vector[T]
    val bottomRight: Vector[T]

    def contains(v: Vector[T])(implicit numericEvidence: Numeric[T]): Boolean = {
      import numericEvidence.mkOrderingOps

      val (xmin, xmax) = xRange
      val (ymin, ymax) = yRange

      xmin <= v.x && v.x <= xmax && ymin <= v.y && v.y <= ymax
    }

    def xRange: (T, T) = upperLeft.x -> bottomRight.x
    def yRange: (T, T) = upperLeft.y -> bottomRight.y

  }

  case class PixelFrame(upperLeft: Pixel, bottomRight: Pixel) extends Frame[Long]
  case class RealFrame(upperLeft: Point, bottomRight: Point) extends Frame[Double]

  case class Scale(realFrame: RealFrame, pixelFrame: PixelFrame)

  object Implicits {

    implicit def tuple2pixel(t: (Long, Long)): Pixel = {
      val (x, y) = t
      Pixel(x, y)
    }

    implicit def tuple2point(t: (Double, Double)): Point = {
      val (x, y) = t
      Point(x, y)
    }

    implicit def real2pixel(p: Point)(implicit scale: Scale): Pixel = {
      import scale._
      import p._

      require(realFrame contains p)

      val (pxMin, pxMax) = pixelFrame.xRange
      val (pyMin, pyMax) = pixelFrame.yRange

      val (xMin, xMax) = realFrame.xRange
      val (yMin, yMax) = realFrame.yRange

      val px = pxMin + ((pxMax-pxMin)*x/(xMax-xMin)).toLong
      val py = pyMin + ((pyMax-pyMin)*y/(yMax-yMin)).toLong

      Pixel(px, py)
    }

    implicit def pixel2real(p: Pixel)(implicit scale: Scale): Point = {
      import scale._
      import p.{x => px, y => py}

      require(pixelFrame contains p)

      val (pxMin, pxMax) = pixelFrame.xRange
      val (pyMin, pyMax) = pixelFrame.yRange

      val (xMin, xMax) = realFrame.xRange
      val (yMin, yMax) = realFrame.yRange

      val x = xMin + (xMax-xMin)*(px.toDouble/(pxMax-pxMin))
      val y = yMin + (yMax-yMin)*(py.toDouble/(pyMax-pyMin))

      Point(x, y)
    }

  }

}

object MandelbrotCalc {

  def iteration(
    x0: Double, y0: Double)(
    x: Double, y: Double)(n: Long): Option[Long] = {

    if(x*x + y*y >= 4.0) None //Ecape condition
    else 

  }

}

object FractalApp extends JSApp {

  import Geom2D._

  def addCanvas(h: Long, w: Long): HTMLCanvasElement = {
    val canvas = document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    canvas.setAttribute("height",  h toString)
    canvas.setAttribute("width",  w toString)
    document.body.appendChild(canvas)
    canvas
  }

  def main(): Unit = {

    import Implicits._

    val drawingAreaSize = (800L, 600L)

    val drawingAreaFrame = PixelFrame(0L -> drawingAreaSize._1, 0L -> drawingAreaSize._2)

    val mandelbrotComplexRange = RealFrame(-2.5 -> 1.0, -1.0 -> 1.0)

    implicit val scale: Scale = Scale(mandelbrotComplexRange, drawingAreaFrame)

    ((addCanvas(_, _)).tupled)(drawingAreaSize)

  }

}
