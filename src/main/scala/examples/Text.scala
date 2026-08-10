package examples

import genart.{
  Color, FontFamily, Frame, Sketch, SketchContext,
  TextAlign, TextBaseline, TextStyle
}
import genart.lib.math.Vec

final class Text(context: SketchContext) extends Sketch:
  private val canvas = context.canvas

  def draw(frame: Frame): Unit =
    canvas.background(Color.rgb(1.0, 1.0, 1.0))
    val scale = math.min(canvas.width / 600.0, canvas.height / 400.0)
    canvas.scoped {
      canvas.translate(Vec(canvas.width / 2.0, canvas.height / 2.0))
      canvas.scale(Vec(scale, scale))
      canvas.noStroke()
      canvas.fill(Color.rgb(0.1, 0.1, 0.1))
      canvas.text(
        "Hello, genart!",
        Vec(0.0, -130.0),
        TextStyle(size = 46.0, bold = true, align = TextAlign.Center)
      )
      for (label, family, x) <- Vector(
        ("Sans serif", FontFamily.SansSerif, -190.0),
        ("Serif", FontFamily.Serif, 0.0),
        ("Monospace", FontFamily.Monospace, 190.0)
      ) do
        canvas.text(
          label,
          Vec(x, -65.0),
          TextStyle(size = 22.0, family = family, align = TextAlign.Center)
        )

      val style = TextStyle(
        size = 26.0,
        align = TextAlign.Center,
        baseline = TextBaseline.Top
      )
      val message = "two lines\nof text"
      val measured = canvas.measureText(message, style)
      canvas.noFill()
      canvas.stroke(Color.rgb(0.8, 0.8, 0.8))
      canvas.strokeWeight(1.0)
      canvas.rect(
        Vec(-measured.width / 2.0, -15.0),
        Vec(measured.width, measured.height)
      )
      canvas.noStroke()
      canvas.fill(Color.hsb(205.0, 0.7, 0.7))
      canvas.text(message, Vec(0.0, -15.0), style)

      canvas.noFill()
      canvas.stroke(Color.rgb(0.1, 0.1, 0.1))
      canvas.strokeWeight(1.0)
      canvas.text(
        "Outlined type",
        Vec(0.0, 135.0),
        TextStyle(size = 40.0, bold = true, align = TextAlign.Center)
      )
    }

object Text extends (SketchContext => Text):
  def apply(context: SketchContext): Text = new Text(context)
