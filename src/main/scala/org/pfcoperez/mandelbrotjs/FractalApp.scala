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

      require(realFrame contains p, s"$p out of $realFrame")

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

      require(pixelFrame contains p, s"$p out of $PixelFrame")

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

object MandelbrotSet {

  def iteration(
    zeroth: (Double, Double)
  )(
    xy: (Double, Double)
  )(n: Long): (Option[(Double, Double)], Long) = {
    val (x, y) = xy
    val (x0, y0) = zeroth

    if(x*x + y*y >= 4.0) None -> n
    else iteration(zeroth)((x*x - y*y + x0, 2.0*x*y+y0))(n+1L)
  }

}

object FractalApp extends JSApp {

  import Geom2D._

  def addCanvas(h: Long, w: Long): HTMLCanvasElement = {
    val canvas =
      document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    canvas.setAttribute("height",  h toString)
    canvas.setAttribute("width",  w toString)
    document.body.appendChild(canvas)
    canvas
  }




  def main(): Unit = {

    import Geom2D.Implicits._

    val drawingAreaSize = (800L, 600L)

    val drawingAreaFrame = PixelFrame(
      0L -> 0L,
      drawingAreaSize
    )

    val mandelbrotComplexRange = RealFrame(-2.5 -> -1.0, 1.0 -> 1.0)

    implicit val scale: Scale = Scale(mandelbrotComplexRange, drawingAreaFrame)

    val renderer = ((addCanvas(_, _)).tupled)(drawingAreaSize).
      getContext("2d").
      asInstanceOf[dom.CanvasRenderingContext2D]

    def drawPoint(point: Point, color: String)(implicit scale: Scale): Unit = {
      renderer.fillStyle = color
      val pixel: Pixel = point
      import pixel._
      renderer.fillRect(x.toDouble, y.toDouble, 1.0, 1.0)
    }

    for(x <- -100 to 100) {
      drawPoint(Point(x/100.0, x/100.0), "black")
    }


  }

}
