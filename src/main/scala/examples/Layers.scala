package examples

import genart.{
  Color,
  Frame,
  Image,
  Sketch,
  SketchContext,
  TextAlign,
  TextBaseline,
  TextStyle
}
import genart.lib.math.Vec
import scala.util.Using

final class Layers(context: SketchContext) extends Sketch:
  private val canvas = context.canvas
  private var stamps = Vector.empty[Image]

  override def setup(): Unit =
    stamps = Using.resource(canvas.surface(160, 160)) { layer =>
      val drawing = layer.canvas
      Vector.tabulate(2) { variant =>
        drawing.clear()
        drawing.noStroke()
        for petal <- 0 until 3 do
          val angle = petal * 2.0 * math.Pi / 3.0 + variant * math.Pi
          drawing.fill(
            Color.hsb(205.0 + variant * 120.0 + petal * 45.0, 0.65, 0.85, 0.65)
          )
          drawing.circle(
            Vec(80.0 + 28.0 * math.cos(angle), 80.0 + 28.0 * math.sin(angle)),
            84.0
          )
        drawing.noFill()
        drawing.stroke(Color.rgb(0.15, 0.15, 0.15))
        drawing.strokeWeight(2.0)
        drawing.circle(Vec(80.0, 80.0), 144.0)
        // Each snapshot survives both clearing and closing its source surface.
        layer.snapshot()
      }
    }

  def draw(frame: Frame): Unit =
    canvas.background(Color.rgb(1.0, 1.0, 1.0))
    canvas.noStroke()
    canvas.fill(Color.rgb(0.93, 0.93, 0.93))
    val tile = math.max(12.0, math.min(canvas.width, canvas.height) / 12.0)
    for
      row <- 0 until math.ceil(canvas.height / tile).toInt
      column <- 0 until math.ceil(canvas.width / tile).toInt
      if (row + column) % 2 == 0
    do canvas.rect(Vec(column * tile, row * tile), Vec(tile, tile))

    val side = math.min(canvas.width * 0.32, canvas.height * 0.28)
    val size = Vec(side, side)
    for (stamp, row) <- stamps.zipWithIndex do
      val y = canvas.height * (0.28 + row * 0.42)
      for copy <- 0 until 2 do
        canvas.scoped {
          canvas.translate(Vec(canvas.width * (0.3 + copy * 0.4), y))
          canvas.rotate(if copy == 0 then -0.22 else 0.22)
          canvas.image(stamp, size * -0.5, size)
        }
      canvas.fill(Color.rgb(0.15, 0.15, 0.15))
      canvas.text(
        if row == 0 then "First snapshot" else "After clear and redraw",
        Vec(canvas.width / 2.0, y + side * 0.75),
        TextStyle(
          size = math.min(14.0, side * 0.12),
          align = TextAlign.Center,
          baseline = TextBaseline.Top
        )
      )

object Layers extends (SketchContext => Layers):
  def apply(context: SketchContext): Layers = new Layers(context)
