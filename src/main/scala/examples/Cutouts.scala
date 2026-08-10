package examples

import genart.{
  Color, FillRule, Frame, Path, Sketch, SketchContext,
  TextAlign, TextBaseline, TextStyle
}
import genart.lib.math.Vec

final class Cutouts(context: SketchContext) extends Sketch:
  private val canvas = context.canvas

  def draw(frame: Frame): Unit =
    val side = math.min(canvas.width * 0.26, canvas.height * 0.5)
    val y = canvas.height * 0.45
    canvas.background(Color.rgb(1.0, 1.0, 1.0))
    canvas.noStroke()
    canvas.fill(Color.rgb(0.9, 0.9, 0.9))
    canvas.rect(
      Vec(0.0, y - side * 0.12),
      Vec(canvas.width.toDouble, side * 0.24)
    )

    for (label, rule, direction, fraction) <- Vector(
      ("Nonzero", FillRule.NonZero, 1.0, 0.18),
      ("Even-odd", FillRule.EvenOdd, 1.0, 0.5),
      ("Reversed", FillRule.NonZero, -1.0, 0.82)
    ) do
      val center = Vec(canvas.width * fraction, y)
      val outer = Path.arc(center, Vec(side, side), 0.0, 2.0 * math.Pi)
      val inner = Path.arc(
        center, Vec(side * 0.45, side * 0.45),
        0.0, direction * 2.0 * math.Pi
      )
      canvas.fill(Color.hsb(205.0, 0.65, 0.8))
      canvas.path(outer ++ inner, rule)
      canvas.fill(Color.rgb(0.15, 0.15, 0.15))
      canvas.text(
        label,
        Vec(center.x, y + side * 0.65),
        TextStyle(
          size = math.min(16.0, side * 0.14),
          align = TextAlign.Center,
          baseline = TextBaseline.Top
        )
      )

object Cutouts extends (SketchContext => Cutouts):
  def apply(context: SketchContext): Cutouts = new Cutouts(context)
