package examples

import genart.{Color, Frame, Sketch, SketchContext}
import genart.lib.math.Vec

final class Polygons(context: SketchContext) extends Sketch:
  private val canvas = context.canvas

  def draw(frame: Frame): Unit =
    val size = math.min(canvas.width / 2.0, canvas.height) * 0.8
    val half = size / 2.0
    val left = canvas.width / 4.0
    val right = canvas.width * 0.75
    val y = canvas.height / 2.0

    canvas.background(Color.rgb(1.0, 1.0, 1.0))
    canvas.stroke(Color.rgb(0.15, 0.15, 0.15))
    canvas.strokeWeight(2.0)
    canvas.fill(Color.hsb(210.0, 0.65, 0.85))
    canvas.triangle(left - half, y + half, left, y - half, left + half, y + half)
    canvas.fill(Color.hsb(330.0, 0.6, 0.85, 0.65))
    canvas.quad(
      Vec(right - size * 0.45, y - size * 0.3),
      Vec(right + size * 0.45, y - size * 0.5),
      Vec(right + size * 0.3, y + size * 0.5),
      Vec(right - size * 0.45, y + size * 0.3)
    )

object Polygons extends (SketchContext => Polygons):
  def apply(context: SketchContext): Polygons = new Polygons(context)
