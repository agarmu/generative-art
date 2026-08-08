package examples

import genart.{Color, Frame, Path, Sketch, SketchContext, StrokeCap, StrokeJoin}
import genart.lib.math.Vec

final class Strokes(context: SketchContext) extends Sketch:
  private val canvas = context.canvas

  def draw(frame: Frame): Unit =
    val scale = math.min(canvas.width / 480.0, canvas.height / 360.0)
    canvas.background(Color.rgb(1.0, 1.0, 1.0))
    canvas.scoped {
      canvas.translate(Vec(canvas.width / 2.0, canvas.height / 2.0))
      canvas.scale(Vec(scale, scale))
      canvas.translate(Vec(-240.0, -180.0))
      canvas.noFill()

      for (cap, index) <- StrokeCap.values.toVector.zipWithIndex do
        val y = 60.0 + index * 48.0
        canvas.stroke(Color.hsb(205.0, 0.65, 0.85))
        canvas.strokeWeight(24.0)
        canvas.strokeCap(cap)
        canvas.line(Vec(120.0, y), Vec(360.0, y))
        canvas.scoped {
          canvas.stroke(Color.rgb(0.15, 0.15, 0.15))
          canvas.strokeWeight(1.0)
          canvas.line(Vec(120.0, y - 20.0), Vec(120.0, y + 20.0))
          canvas.line(Vec(360.0, y - 20.0), Vec(360.0, y + 20.0))
        }

      for (join, index) <- StrokeJoin.values.toVector.zipWithIndex do
        val x = 100.0 + index * 140.0
        canvas.stroke(Color.hsb(330.0, 0.6, 0.8))
        canvas.strokeWeight(24.0)
        canvas.strokeCap(StrokeCap.Butt)
        canvas.strokeJoin(join)
        canvas.path(
          Path(Vec(x - 45.0, 310.0))
            .lineTo(Vec(x, 230.0))
            .lineTo(Vec(x + 45.0, 310.0))
        )
    }

object Strokes extends (SketchContext => Strokes):
  def apply(context: SketchContext): Strokes = new Strokes(context)
