package examples

import genart.{
  Color, Frame, Key, KeyEvent, Sketch, SketchContext, Step,
  TextAlign, TextBaseline, TextStyle
}
import genart.lib.math.{Vec, clamp}

final class ScreenSaver(context: SketchContext) extends Sketch:
  private val canvas = context.canvas
  private val background = Color.rgb(0.0, 0.0, 0.0)
  private var seconds = 0.0
  private var position = Vec(0.0, 0.0)
  private var velocity = Vec(95.0, 72.0)
  private var paused = false

  private def size: Vec[2] =
    val side = math.min(120.0, math.min(canvas.width, canvas.height) * 0.25)
    val width = side * math.exp(0.3 * math.sin(seconds))
    Vec(width, side * side / width)

  override def setup(): Unit =
    seconds = 0.0
    paused = false
    velocity = Vec(95.0, 72.0)
    position = (Vec(canvas.width.toDouble, canvas.height.toDouble) - size) * 0.5

  override def key(event: KeyEvent): Unit = event match
    case KeyEvent.Pressed(key, modifiers, false)
        if !modifiers.control && !modifiers.alt && !modifiers.meta =>
      key match
        case Key.Space          => paused = !paused
        case Key.Character("r") => setup()
        case Key.ArrowLeft  => velocity = Vec(-velocity.x.abs, velocity.y)
        case Key.ArrowRight => velocity = Vec(velocity.x.abs, velocity.y)
        case Key.ArrowUp    => velocity = Vec(velocity.x, -velocity.y.abs)
        case Key.ArrowDown  => velocity = Vec(velocity.x, velocity.y.abs)
        case _ => ()
    case _ => ()

  override def update(step: Step): Unit =
    if !paused then
      seconds += step.deltaSeconds
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
    canvas.fill(Color.rgb(0.55, 0.55, 0.55))
    canvas.text(
      s"${if paused then "Paused" else "Space: pause"}  ·  R: reset  ·  Arrows: steer",
      Vec(canvas.width / 2.0, canvas.height - 12.0),
      TextStyle(
        size = math.min(12.0, canvas.width / 35.0),
        align = TextAlign.Center,
        baseline = TextBaseline.Bottom
      )
    )

object ScreenSaver extends (SketchContext => ScreenSaver):
  def apply(context: SketchContext): ScreenSaver = new ScreenSaver(context)
