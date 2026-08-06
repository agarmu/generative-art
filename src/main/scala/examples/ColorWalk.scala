package examples

import genart.{Color, Frame, Sketch, SketchContext, Step}
import genart.lib.math.floorMod
import genart.lib.random.Rng

final class ColorWalk(context: SketchContext) extends Sketch:
  private val canvas = context.canvas
  private val background = Color.rgb(1.0, 1.0, 1.0)
  private val typicalSpeed = math.toRadians(60.0)
  private val persistenceSeconds = 2.0
  private var rng = Rng(context.seed)
  private var angle = 0.0
  private var angularVelocity = 0.0

  override def setup(): Unit =
    val (initialAngle, next) = rng.uniform(0.0, 2.0 * math.Pi)
    val (direction, remaining) = next.nextInt(2)
    angle = initialAngle
    angularVelocity = if direction == 0 then typicalSpeed else -typicalSpeed
    rng = remaining

  override def update(step: Step): Unit =
    val (normal, next) = rng.nextGaussian
    val decay = math.exp(-step.deltaSeconds / persistenceSeconds)
    val previousVelocity = angularVelocity
    angularVelocity = angularVelocity * decay +
      typicalSpeed * math.sqrt(1.0 - decay * decay) * normal
    angle += (previousVelocity + angularVelocity) / 2.0 * step.deltaSeconds
    rng = next

  def draw(frame: Frame): Unit =
    val hue = math.toDegrees(angle)
    val size = math.min(canvas.width / 2.0, canvas.height) * 0.7
    val radius = size / 2.0
    val squareX = canvas.width / 4.0
    val wheelX = canvas.width * 0.75
    val y = canvas.height / 2.0

    canvas.background(background)
    canvas.noStroke()
    canvas.fill(Color.hsb(hue, 0.7, 0.85))
    canvas.rect(squareX - radius, y - radius, size, size)

    canvas.strokeWeight(size * 0.012)
    for degrees <- 0 until 360 do
      val angle = math.toRadians(degrees.toDouble)
      canvas.stroke(Color.hsb(degrees.toDouble, 0.7, 0.85))
      canvas.line(
        wheelX,
        y,
        wheelX + radius * math.cos(angle),
        y + radius * math.sin(angle)
      )

    canvas.noFill()
    canvas.stroke(background)
    canvas.strokeWeight(size * 0.04)
    canvas.circle(wheelX, y, size * 1.04)

    canvas.noStroke()
    canvas.fill(background)
    canvas.circle(wheelX, y, size * 0.92)

    val markerAngle = floorMod(angle, 2.0 * math.Pi)
    canvas.noFill()
    canvas.stroke(Color.rgb(0.1, 0.1, 0.1))
    canvas.strokeWeight(size * 0.008)
    canvas.circle(
      wheelX + radius * 0.96 * math.cos(markerAngle),
      y + radius * 0.96 * math.sin(markerAngle),
      size * 0.1
    )

object ColorWalk extends (SketchContext => ColorWalk):
  def apply(context: SketchContext): ColorWalk = new ColorWalk(context)
