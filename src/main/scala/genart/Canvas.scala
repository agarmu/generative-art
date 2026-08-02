package genart

import genart.lib.requireFinite
import genart.lib.math.Vec
import scala.util.Using

trait Canvas:
  def width: Int
  def height: Int

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

  final def rect(x: Double, y: Double, width: Double, height: Double): Unit =
    requireFinite(x, "x")
    requireFinite(y, "y")
    requireFinite(width, "width")
    requireFinite(height, "height")
    require(
      width >= 0.0 && height >= 0.0,
      "rectangle dimensions must be nonnegative"
    )
    drawRect(x, y, width, height)

  final def line(x1: Double, y1: Double, x2: Double, y2: Double): Unit =
    requireFinite(x1, "x1")
    requireFinite(y1, "y1")
    requireFinite(x2, "x2")
    requireFinite(y2, "y2")
    drawLine(x1, y1, x2, y2)

  final def line(from: Vec[2], to: Vec[2]): Unit =
    line(from.x, from.y, to.x, to.y)

  final def translate(x: Double, y: Double): Unit =
    requireFinite(x, "x")
    requireFinite(y, "y")
    translateBy(x, y)

  final def translate(offset: Vec[2]): Unit = translate(offset.x, offset.y)

  final def rotate(radians: Double): Unit =
    requireFinite(radians, "radians")
    rotateBy(radians)

  final def scale(x: Double, y: Double): Unit =
    requireFinite(x, "x")
    requireFinite(y, "y")
    scaleBy(x, y)

  final def scoped[A](body: => A): A =
    pushState()
    Using.resource(() => popState())(_ => body)(using Canvas.releaseScope)

  protected def setStrokeColor(color: Color): Unit
  protected def setFillColor(color: Color): Unit
  protected def disableStroke(): Unit
  protected def disableFill(): Unit
  protected def setStrokeWeight(weight: Double): Unit
  protected def clearSurface(color: Color): Unit
  protected def drawRect(
      x: Double,
      y: Double,
      width: Double,
      height: Double
  ): Unit
  protected def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit
  protected def translateBy(x: Double, y: Double): Unit
  protected def rotateBy(radians: Double): Unit
  protected def scaleBy(x: Double, y: Double): Unit
  protected def pushState(): Unit
  protected def popState(): Unit

object Canvas:
  private val releaseScope: Using.Releasable[() => Unit] = release => release()
