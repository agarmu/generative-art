package examples

import genart.{
  BlendMode,
  Color,
  Frame,
  Sketch,
  SketchContext,
  TextAlign,
  TextStyle
}
import genart.lib.math.Vec

final class Blending(context: SketchContext) extends Sketch:
  private val canvas = context.canvas

  def draw(frame: Frame): Unit =
    canvas.background(Color.rgb(1.0, 1.0, 1.0))
    val scale = math.min(canvas.width / 640.0, canvas.height / 360.0)
    canvas.scoped {
      canvas.translate(Vec(canvas.width / 2.0, canvas.height / 2.0))
      canvas.scale(Vec(scale, scale))
      canvas.translate(Vec(-320.0, -180.0))
      canvas.noStroke()
      for (mode, index) <- BlendMode.values.toVector.zipWithIndex do
        val origin = Vec((index % 4) * 160.0, (index / 4) * 180.0)
        canvas.scoped {
          canvas.translate(origin)
          canvas.fill(Color.hsb(205.0, 0.75, 0.8))
          canvas.circle(Vec(60.0, 70.0), 80.0)
          canvas.blendMode(mode)
          canvas.fill(Color.hsb(335.0, 0.7, 0.9))
          canvas.circle(Vec(100.0, 70.0), 80.0)
        }
        canvas.fill(Color.rgb(0.15, 0.15, 0.15))
        canvas.text(
          mode.toString,
          origin + Vec(80.0, 145.0),
          TextStyle(size = 16.0, align = TextAlign.Center)
        )
    }

object Blending extends (SketchContext => Blending):
  def apply(context: SketchContext): Blending = new Blending(context)
