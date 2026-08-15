package examples

import genart.{
  Color,
  Frame,
  Gradient,
  Sketch,
  SketchContext,
  TextAlign,
  TextStyle
}
import genart.lib.math.Vec

final class Gradients(context: SketchContext) extends Sketch:
  private val canvas = context.canvas
  private val linear = Gradient.Linear(
    Vec(0.0, 0.0),
    Vec(220.0, 0.0),
    Vector(
      Gradient.Stop(0.0, Color.hsb(205.0, 0.8, 0.85)),
      Gradient.Stop(0.5, Color.hsb(275.0, 0.6, 0.9)),
      Gradient.Stop(1.0, Color.hsb(335.0, 0.7, 0.95))
    )
  )
  private val radial = Gradient.Radial(
    Vec(110.0, 70.0),
    90.0,
    Vector(
      Gradient.Stop(0.0, Color.hsb(45.0, 0.3, 1.0)),
      Gradient.Stop(1.0, Color.hsb(15.0, 0.85, 0.85))
    )
  )

  def draw(frame: Frame): Unit =
    canvas.background(Color.rgb(1.0, 1.0, 1.0))
    val scale = math.min(canvas.width / 560.0, canvas.height / 320.0)
    canvas.scoped {
      canvas.translate(Vec(canvas.width / 2.0, canvas.height / 2.0))
      canvas.scale(Vec(scale, scale))
      canvas.translate(Vec(-280.0, -160.0))
      canvas.noStroke()
      for (label, gradient, x) <- Vector(
          ("Linear", linear, 30.0),
          ("Radial", radial, 310.0)
        )
      do
        canvas.scoped {
          // Setting paint before a transform keeps the gradient local to drawing.
          canvas.fill(gradient)
          canvas.translate(Vec(x, 35.0))
          canvas.rect(Vec(0.0, 0.0), Vec(220.0, 140.0))
          canvas.fill(Color.rgb(0.15, 0.15, 0.15))
          canvas.text(
            label,
            Vec(110.0, 210.0),
            TextStyle(size = 18.0, align = TextAlign.Center)
          )
          canvas.noFill()
          canvas.stroke(gradient match
            case radial: Gradient.Radial =>
              radial.copy(center = Vec(110.0, 240.0), radius = 110.0)
            case other => other)
          canvas.strokeWeight(6.0)
          canvas.line(Vec(0.0, 240.0), Vec(220.0, 240.0))
        }
    }

object Gradients extends (SketchContext => Gradients):
  def apply(context: SketchContext): Gradients = new Gradients(context)
