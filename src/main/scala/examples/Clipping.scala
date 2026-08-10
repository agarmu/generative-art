package examples

import genart.{Color, FillRule, Frame, Path, Sketch, SketchContext, Step}
import genart.lib.math.Vec

final class Clipping(context: SketchContext) extends Sketch:
  private val canvas = context.canvas
  private var seconds = 0.0

  override def update(step: Step): Unit = seconds = step.seconds

  def draw(frame: Frame): Unit =
    val side = math.min(canvas.width, canvas.height) * 0.75
    val center = Vec(canvas.width / 2.0, canvas.height / 2.0)
    val outer = Path.arc(center, Vec(side, side), 0.0, 2.0 * math.Pi)
    val inner = Path.arc(center, Vec(side * 0.45, side * 0.45), 0.0, 2.0 * math.Pi)
    canvas.background(Color.rgb(1.0, 1.0, 1.0))
    canvas.noStroke()
    canvas.clipped(outer ++ inner, FillRule.EvenOdd) {
      canvas.translate(center)
      canvas.rotate(seconds * 0.15 - 0.4)
      val stripe = side / 8.0
      for index <- -12 to 12 do
        canvas.fill(Color.hsb(index * 25.0 + seconds * 12.0, 0.55, 0.85))
        canvas.rect(Vec(index * stripe, -side), Vec(stripe, side * 2.0))
    }

object Clipping extends (SketchContext => Clipping):
  def apply(context: SketchContext): Clipping = new Clipping(context)
