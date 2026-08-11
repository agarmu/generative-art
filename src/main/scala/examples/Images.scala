package examples

import genart.{Color, Frame, Image, Sketch, SketchContext}
import genart.lib.math.Vec

final class Images(context: SketchContext) extends Sketch:
  private val canvas = context.canvas
  private var picture = Option.empty[Image]

  override def setup(): Unit =
    val w = canvas.width.toDouble
    val h = canvas.height.toDouble
    canvas.background(Color.hsb(45.0, 0.12, 0.98))
    canvas.noStroke()
    canvas.fill(Color.hsb(15.0, 0.65, 0.95))
    canvas.circle(Vec(w * 0.62, h * 0.38), h * 0.46)
    canvas.fill(Color.hsb(195.0, 0.6, 0.7))
    canvas.rect(Vec(w * 0.2, h * 0.55), Vec(w * 0.6, h * 0.25))
    picture = Some(canvas.snapshot())

  def draw(frame: Frame): Unit =
    canvas.background(Color.rgb(1.0, 1.0, 1.0))
    picture.foreach { source =>
      val aspect = source.width.toDouble / source.height
      val width = math.min(canvas.width * 0.4, canvas.height * 0.75 * aspect)
      val size = Vec(width, width / aspect)
      canvas.scoped {
        canvas.translate(Vec(canvas.width * 0.28, canvas.height * 0.5))
        canvas.rotate(-0.12)
        canvas.image(source, size * -0.5, size)
      }
      canvas.scoped {
        canvas.translate(Vec(canvas.width * 0.72, canvas.height * 0.5))
        canvas.rotate(0.12)
        canvas.image(
          source,
          size * -0.5,
          size,
          Vec(source.width * 0.35, source.height * 0.15),
          Vec(source.width * 0.5, source.height * 0.5)
        )
      }
    }

object Images extends (SketchContext => Images):
  def apply(context: SketchContext): Images = new Images(context)
