package genart

import genart.lib.requireFinite
import genart.lib.math.Vec
import scala.util.Using

trait Canvas:
  def width: Int
  def height: Int
  def snapshot(): Image

  final def stroke(color: Color): Unit = setStrokeColor(color)
  final def fill(color: Color): Unit = setFillColor(color)
  final def noStroke(): Unit = disableStroke()
  final def noFill(): Unit = disableFill()

  final def strokeWeight(weight: Double): Unit =
    requireFinite(weight, "strokeWeight")
    require(weight >= 0.0, "strokeWeight must be nonnegative")
    setStrokeWeight(weight)

  final def style(value: Style): Unit =
    value.stroke.fold(noStroke())(stroke)
    value.fill.fold(noFill())(fill)
    strokeWeight(value.strokeWeight)

  final def background(color: Color): Unit =
    require(color.alpha == 1.0, "background must be opaque")
    clearSurface(color)

  final def rect(position: Vec[2], size: Vec[2]): Unit =
    validatePoint(position)
    validateSize(size)
    drawRect(position, size)

  final def ellipse(center: Vec[2], size: Vec[2]): Unit =
    validatePoint(center)
    validateSize(size)
    drawEllipse(center, size)

  final def circle(center: Vec[2], diameter: Double): Unit =
    ellipse(center, Vec(diameter, diameter))

  final def triangle(a: Vec[2], b: Vec[2], c: Vec[2]): Unit =
    validatePoint(a)
    validatePoint(b)
    validatePoint(c)
    drawTriangle(a, b, c)

  final def quad(a: Vec[2], b: Vec[2], c: Vec[2], d: Vec[2]): Unit =
    validatePoint(a)
    validatePoint(b)
    validatePoint(c)
    validatePoint(d)
    drawQuad(a, b, c, d)

  final def line(from: Vec[2], to: Vec[2]): Unit =
    validatePoint(from)
    validatePoint(to)
    drawLine(from, to)

  final def translate(offset: Vec[2]): Unit =
    validatePoint(offset)
    translateBy(offset)

  final def rotate(radians: Double): Unit =
    requireFinite(radians, "radians")
    rotateBy(radians)

  final def scale(factors: Vec[2]): Unit =
    validatePoint(factors)
    scaleBy(factors)

  final def scoped[A](body: => A): A =
    pushState()
    Using.resource(() => popState())(_ => body)(using Canvas.releaseScope)

  private def validatePoint(point: Vec[2]): Unit =
    requireFinite(point.x, "x")
    requireFinite(point.y, "y")

  private def validateSize(size: Vec[2]): Unit =
    validatePoint(size)
    require(size.x >= 0.0 && size.y >= 0.0, "dimensions must be nonnegative")

  protected def setStrokeColor(color: Color): Unit
  protected def setFillColor(color: Color): Unit
  protected def disableStroke(): Unit
  protected def disableFill(): Unit
  protected def setStrokeWeight(weight: Double): Unit
  protected def clearSurface(color: Color): Unit
  protected def drawRect(position: Vec[2], size: Vec[2]): Unit
  protected def drawLine(from: Vec[2], to: Vec[2]): Unit
  protected def drawEllipse(center: Vec[2], size: Vec[2]): Unit
  protected def drawTriangle(a: Vec[2], b: Vec[2], c: Vec[2]): Unit
  protected def drawQuad(a: Vec[2], b: Vec[2], c: Vec[2], d: Vec[2]): Unit
  protected def translateBy(offset: Vec[2]): Unit
  protected def rotateBy(radians: Double): Unit
  protected def scaleBy(factors: Vec[2]): Unit
  protected def pushState(): Unit
  protected def popState(): Unit

object Canvas:
  private val releaseScope: Using.Releasable[() => Unit] = release => release()
