package genart.p5

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("p5", JSImport.Default)
private[p5] class P5(sketch: js.Function1[P5, Unit], container: dom.Element)
    extends js.Object:
  var setup: js.Function0[Unit] = js.native
  var draw: js.Function0[Unit] = js.native
  val width: Int = js.native
  val height: Int = js.native
  val canvas: dom.HTMLCanvasElement = js.native
  val drawingContext: dom.CanvasRenderingContext2D = js.native
  val RGB: String = js.native
  val BLEND: String = js.native
  val CORNER: String = js.native
  val RADIANS: String = js.native

  def createCanvas(width: Int, height: Int): js.Any = js.native
  def resizeCanvas(width: Int, height: Int, noRedraw: Boolean): Unit = js.native
  def pixelDensity(value: Int): Unit = js.native
  def frameRate(value: Double): Unit = js.native
  def colorMode(mode: String, maximum: Double): Unit = js.native
  def blendMode(mode: String): Unit = js.native
  def rectMode(mode: String): Unit = js.native
  def angleMode(mode: String): Unit = js.native
  def stroke(red: Double, green: Double, blue: Double, alpha: Double): Unit =
    js.native
  def fill(red: Double, green: Double, blue: Double, alpha: Double): Unit =
    js.native
  def noStroke(): Unit = js.native
  def noFill(): Unit = js.native
  def strokeWeight(weight: Double): Unit = js.native
  def background(red: Double, green: Double, blue: Double): Unit = js.native
  def rect(x: Double, y: Double, width: Double, height: Double): Unit =
    js.native
  def line(x1: Double, y1: Double, x2: Double, y2: Double): Unit = js.native
  def translate(x: Double, y: Double): Unit = js.native
  def rotate(radians: Double): Unit = js.native
  def scale(x: Double, y: Double): Unit = js.native
  def push(): Unit = js.native
  def pop(): Unit = js.native
  def resetMatrix(): Unit = js.native
  def noLoop(): Unit = js.native
  def remove(): js.Promise[Unit] = js.native
