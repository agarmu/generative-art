import processing.core.PApplet
import processing.core.PApplet.* // math functions
import processing.core.PConstants.*

class Sketch extends PApplet:

  override def settings(): Unit =
    fullScreen()

  override def setup(): Unit =
    colorMode(HSB, 360f, 100f, 100f)
    noStroke()

  override def draw(): Unit =
    background(255 / 2f)
    val cx = width / 2f
    val cy = height / 2f
    val r = min(width, height) / 2.5f
    val rotation = frameCount * 0.003f
    val slices = 10
    val step = TWO_PI / slices

    for i <- 0 until slices do
      val angle = i * step + rotation
      val hue = (i.toFloat / slices) * 360f
      fill(hue, 85f, 90f)
      arc(cx, cy, r * 2, r * 2, angle, angle + step, PIE)

object Sketch:
  def main(args: Array[String]): Unit =
    PApplet.main("Sketch")
