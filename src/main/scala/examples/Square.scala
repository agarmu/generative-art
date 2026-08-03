package examples

import genart.{Color, Frame, Sketch, SketchContext}

final class Square(context: SketchContext) extends Sketch:
  private val canvas = context.canvas

  def draw(frame: Frame): Unit =
    val size = math.min(canvas.width, canvas.height) * 0.4
    val x = (canvas.width - size) / 2.0
    val y = (canvas.height - size) / 2.0

    canvas.background(Color.rgb(0.96, 0.94, 0.90))
    canvas.noStroke()
    canvas.fill(Color.rgb(0.20, 0.45, 0.65))
    canvas.rect(x, y, size, size)

object Square extends (SketchContext => Square):
  def apply(context: SketchContext): Square = new Square(context)
