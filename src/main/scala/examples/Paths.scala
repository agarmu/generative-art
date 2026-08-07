package examples

import genart.{ArcMode, Color, Frame, Path, Sketch, SketchContext}
import genart.lib.math.Vec

final class Paths(context: SketchContext) extends Sketch:
  private val canvas = context.canvas

  def draw(frame: Frame): Unit =
    val scale = math.min(canvas.width / 480.0, canvas.height / 360.0)
    canvas.background(Color.rgb(1.0, 1.0, 1.0))
    canvas.scoped {
      canvas.translate(Vec(canvas.width / 2.0, canvas.height / 2.0))
      canvas.scale(Vec(scale, scale))
      canvas.translate(Vec(-240.0, -180.0))
      canvas.stroke(Color.rgb(0.15, 0.15, 0.15))
      canvas.strokeWeight(3.0)

      for (mode, index) <- ArcMode.values.toVector.zipWithIndex do
        canvas.fill(Color.hsb(200.0 + index * 60.0, 0.6, 0.9, 0.6))
        canvas.arc(
          Vec(100.0 + index * 140.0, 70.0),
          Vec(90.0, 90.0),
          -math.Pi / 3.0,
          math.Pi * 1.5,
          mode
        )

      canvas.noFill()
      canvas.stroke(Color.hsb(205.0, 0.75, 0.75))
      canvas.bezier(
        Vec(40.0, 200.0),
        Vec(140.0, 100.0),
        Vec(340.0, 260.0),
        Vec(440.0, 160.0)
      )

      canvas.fill(Color.hsb(330.0, 0.6, 0.85, 0.4))
      canvas.stroke(Color.hsb(330.0, 0.7, 0.65))
      canvas.path(
        Path(Vec(100.0, 310.0))
          .lineTo(Vec(130.0, 260.0))
          .quadraticTo(Vec(240.0, 200.0), Vec(350.0, 260.0))
          .arcTo(Vec(350.0, 290.0), Vec(60.0, 60.0), -math.Pi / 2.0, math.Pi)
          .bezierTo(Vec(280.0, 350.0), Vec(180.0, 350.0), Vec(100.0, 310.0))
          .close
      )
    }

object Paths extends (SketchContext => Paths):
  def apply(context: SketchContext): Paths = new Paths(context)
