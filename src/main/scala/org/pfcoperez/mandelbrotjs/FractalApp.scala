package org.pfcoperez.mandelbrotjs

import scala.scalajs.js.JSApp

import org.scalajs.dom
import dom.document
import dom.raw.HTMLCanvasElement

trait Geom2D {

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

object FractalApp extends JSApp {

  def addCanvas(h: Int, w: Int): HTMLCanvasElement = {
    val canvas = document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    canvas.setAttribute("height",  h toString)
    canvas.setAttribute("width",  w toString)
    document.body.appendChild(canvas)
    canvas
  }

  def main(): Unit = {
    println("hi")
  }

}
