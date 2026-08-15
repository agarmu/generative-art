package examples

import genart.{Color, Frame, Image, Sketch, SketchContext, TextAlign, TextStyle}
import genart.lib.math.Vec
import scala.util.Using

final class Tinting(context: SketchContext) extends Sketch:
  private val canvas = context.canvas
  private var picture = Option.empty[Image]

  override def setup(): Unit =
    picture = Some(Using.resource(canvas.surface(120, 120)) { layer =>
      layer.canvas.noStroke()
      layer.canvas.fill(Color.rgb(1.0, 1.0, 1.0))
      layer.canvas.circle(Vec(60.0, 50.0), 80.0)
      layer.canvas.fill(Color.rgb(0.45, 0.45, 0.45))
      layer.canvas.rect(Vec(25.0, 60.0), Vec(70.0, 40.0))
      layer.snapshot()
    })

  def draw(frame: Frame): Unit =
    canvas.background(Color.rgb(0.9, 0.9, 0.9))
    val scale = math.min(canvas.width / 560.0, canvas.height / 220.0)
    canvas.scoped {
      canvas.translate(Vec(canvas.width / 2.0, canvas.height / 2.0))
      canvas.scale(Vec(scale, scale))
      canvas.translate(Vec(-280.0, -110.0))
      picture.foreach { source =>
        for (label, color, index) <- Vector(
            ("Original", Color.rgb(1.0, 1.0, 1.0), 0),
            ("Warm", Color.rgb(1.0, 0.4, 0.2), 1),
            ("Cool", Color.rgb(0.2, 0.6, 1.0), 2),
            ("Half alpha", Color.rgb(1.0, 1.0, 1.0, 0.5), 3)
          )
        do
          canvas.scoped {
            canvas.tint(color)
            canvas.image(source, Vec(10.0 + index * 140.0, 25.0))
          }
          canvas.noStroke()
          canvas.fill(Color.rgb(0.15, 0.15, 0.15))
          canvas.text(
            label,
            Vec(70.0 + index * 140.0, 185.0),
            TextStyle(size = 16.0, align = TextAlign.Center)
          )
      }
    }

object Tinting extends (SketchContext => Tinting):
  def apply(context: SketchContext): Tinting = new Tinting(context)
