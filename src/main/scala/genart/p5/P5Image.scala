package genart.p5

import genart.{Color, Image}
import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("WeakMap")
private[p5] class ImageCache extends js.Object:
  def get(source: Image): js.UndefOr[dom.HTMLCanvasElement] = js.native
  def set(source: Image, surface: dom.HTMLCanvasElement): ImageCache = js.native

private[p5] final class P5Image(
    val surface: dom.HTMLCanvasElement,
    pixels: dom.ImageData
) extends Image(pixels.width, pixels.height):
  protected def read(x: Int, y: Int): Color =
    val offset = (y * width + x) * 4
    Color.rgb(
      pixels.data(offset) / 255.0,
      pixels.data(offset + 1) / 255.0,
      pixels.data(offset + 2) / 255.0,
      pixels.data(offset + 3) / 255.0
    )

private[p5] object P5Image:
  def surface(width: Int, height: Int): dom.HTMLCanvasElement =
    val canvas =
      dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
    canvas.width = width
    canvas.height = height
    canvas

  def context(surface: dom.HTMLCanvasElement): dom.CanvasRenderingContext2D =
    surface.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  def capture(source: dom.HTMLCanvasElement): P5Image =
    val captured = surface(source.width, source.height)
    val drawing = context(captured)
    drawing.drawImage(source, 0.0, 0.0)
    new P5Image(
      captured,
      drawing.getImageData(0.0, 0.0, source.width, source.height)
    )

  def upload(source: Image): dom.HTMLCanvasElement =
    val canvas = surface(source.width, source.height)
    val drawing = context(canvas)
    val pixels = drawing.createImageData(source.width, source.height)
    for y <- 0 until source.height; x <- 0 until source.width do
      val color = source(x, y)
      val offset = (y * source.width + x) * 4
      pixels.data(offset) = math.round(color.red * 255.0).toInt
      pixels.data(offset + 1) = math.round(color.green * 255.0).toInt
      pixels.data(offset + 2) = math.round(color.blue * 255.0).toInt
      pixels.data(offset + 3) = math.round(color.alpha * 255.0).toInt
    drawing.putImageData(pixels, 0.0, 0.0)
    canvas
