package genart

import genart.lib.math.Vec

trait Sketch:
  def setup(): Unit = ()
  def draw(): Unit
  def resized(): Unit = ()
  def dispose(): Unit = ()

trait Canvas:
  def width: Int
  def height: Int

  def stroke(color: Color): Unit
  def fill(color: Color): Unit
  def noStroke(): Unit
  def noFill(): Unit
  def strokeWeight(weight: Double): Unit

  final def style(value: Style): Unit =
    value.stroke match
      case Some(color) => stroke(color)
      case None        => noStroke()
    value.fill match
      case Some(color) => fill(color)
      case None        => noFill()
    strokeWeight(value.strokeWeight)

  def background(color: Color): Unit

  def rect(x: Double, y: Double, width: Double, height: Double): Unit
  def line(x1: Double, y1: Double, x2: Double, y2: Double): Unit

  final def line(from: Vec[2], to: Vec[2]): Unit =
    line(from.x, from.y, to.x, to.y)

  def translate(x: Double, y: Double): Unit
  final def translate(offset: Vec[2]): Unit = translate(offset.x, offset.y)

  def rotate(radians: Double): Unit
  def scale(x: Double, y: Double): Unit

  final def scoped[A](body: => A): A =
    pushState()
    try body
    finally popState()

  /** Save both drawing styles and transforms; support nested scopes. */
  protected def pushState(): Unit
  /** Restore the styles and transforms saved by the matching pushState. */
  protected def popState(): Unit
