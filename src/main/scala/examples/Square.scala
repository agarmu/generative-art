package examples

import genart.{Color, Frame, Sketch, SketchContext, Step}
import genart.lib.random.Rng

final class Square(context: SketchContext) extends Sketch:
  private val canvas = context.canvas
  private val background = Color.rgb(0.96, 0.94, 0.90)
  private var rng = Rng(context.seed)
  private var hue = 0.0
  private val hueVariation = 12.0

  override def setup(): Unit =
    val (initial, next) = rng.uniform(0.0, 360.0)
    hue = initial
    rng = next
    canvas.background(background)

  override def resized(): Unit = canvas.background(background)

  override def update(step: Step): Unit =
    val (normal, next) = rng.nextGaussian
    hue += normal * hueVariation * math.sqrt(step.deltaSeconds)
    rng = next

  def draw(frame: Frame): Unit =
    val size = math.min(canvas.width, canvas.height) * 0.4
    val x = (canvas.width - size) / 2.0
    val y = (canvas.height - size) / 2.0

    canvas.noStroke()
    canvas.fill(Color.hsb(hue, 0.7, 0.85))
    canvas.rect(x, y, size, size)

object Square extends (SketchContext => Square):
  def apply(context: SketchContext): Square = new Square(context)
