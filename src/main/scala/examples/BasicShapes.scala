package examples

import genart.{Color, Frame, Sketch, SketchContext}

final class BasicShapes(context: SketchContext) extends Sketch:
  private val canvas = context.canvas

  def draw(frame: Frame): Unit =
    val size = math.min(canvas.width, canvas.height)
    val x = canvas.width / 2.0
    val y = canvas.height / 2.0

    canvas.background(Color.rgb(1.0, 1.0, 1.0))
    canvas.noStroke()
    canvas.fill(Color.hsb(205.0, 0.65, 0.85, 0.65))
    canvas.ellipse(x, y, size * 0.75, size * 0.4)
    canvas.fill(Color.hsb(335.0, 0.65, 0.9, 0.65))
    canvas.ellipse(x, y, size * 0.4, size * 0.75)
    canvas.noFill()
    canvas.stroke(Color.rgb(0.15, 0.15, 0.15))
    canvas.strokeWeight(2.0)
    canvas.circle(x, y, size * 0.6)

    val side = size * 0.65
    val offset = size * 0.1
    canvas.noStroke()
    canvas.fill(Color.hsb(40.0, 0.65, 0.9, 0.4))
    canvas.rect(x + offset - side / 2.0, y + offset - side / 2.0, side, side)

object BasicShapes extends (SketchContext => BasicShapes):
  def apply(context: SketchContext): BasicShapes = new BasicShapes(context)
