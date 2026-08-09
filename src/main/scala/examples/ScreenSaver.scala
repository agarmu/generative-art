package examples

import genart.{Color, Frame, Sketch, SketchContext, Step}
import genart.lib.math.{Vec, clamp}

final class ScreenSaver(context: SketchContext) extends Sketch:
  private val canvas = context.canvas
  private val background = Color.rgb(0.0, 0.0, 0.0)
  private var seconds = 0.0
  private var position = Vec(0.0, 0.0)
  private var velocity = Vec(95.0, 72.0)

  private def size: Vec[2] =
    val side = math.min(120.0, math.min(canvas.width, canvas.height) * 0.25)
    val width = side * math.exp(0.3 * math.sin(seconds))
    Vec(width, side * side / width)

  override def setup(): Unit =
    position = (Vec(canvas.width.toDouble, canvas.height.toDouble) - size) * 0.5

  override def update(step: Step): Unit =
    seconds = step.seconds
    val limit = Vec(canvas.width.toDouble, canvas.height.toDouble) - size
    val next = position + velocity * step.deltaSeconds
    val hitX = next.x <= 0.0 || next.x >= limit.x
    val hitY = next.y <= 0.0 || next.y >= limit.y
    position = Vec(clamp(next.x, 0.0, limit.x), clamp(next.y, 0.0, limit.y))
    velocity = Vec(
      if hitX then -velocity.x else velocity.x,
      if hitY then -velocity.y else velocity.y
    )

  override def resized(): Unit =
    val limit = Vec(canvas.width.toDouble, canvas.height.toDouble) - size
    position = Vec(clamp(position.x, 0.0, limit.x), clamp(position.y, 0.0, limit.y))

  def draw(frame: Frame): Unit =
    canvas.background(background)
    canvas.noStroke()
    canvas.fill(Color.rgb(1.0, 1.0, 1.0))
    canvas.rect(position, size)

object ScreenSaver extends (SketchContext => ScreenSaver):
  def apply(context: SketchContext): ScreenSaver = new ScreenSaver(context)
