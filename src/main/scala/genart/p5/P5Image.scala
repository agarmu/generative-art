package genart.p5

import genart.{Color, Image}
import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.Uint8ClampedArray

@js.native
@JSGlobal("WeakMap")
private[p5] class ImageCache extends js.Object:
  def get(source: Image): js.UndefOr[dom.HTMLCanvasElement] = js.native
  def set(source: Image, surface: dom.HTMLCanvasElement): ImageCache = js.native

final class P5Image private[p5] (
    private[p5] val surface: dom.HTMLCanvasElement,
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

object P5Image:
  def from(source: dom.HTMLImageElement): Image =
    require(source.complete, "image must finish loading before conversion")
    copy(source.naturalWidth, source.naturalHeight)(
      _.drawImage(source, 0.0, 0.0)
    )

  def from(source: dom.ImageBitmap): Image =
    copy(source.width.toInt, source.height.toInt) { drawing =>
      drawing.asInstanceOf[BitmapDrawingContext].drawImage(source, 0.0, 0.0)
    }

  def from(source: dom.HTMLCanvasElement): Image = capture(source)

  def from(source: dom.ImageData): Image =
    val pixels = new dom.ImageData(
      new Uint8ClampedArray(source.data),
      source.width,
      source.height
    )
    val canvas = surface(source.width, source.height)
    context(canvas).putImageData(pixels, 0.0, 0.0)
    new P5Image(canvas, pixels)

  private def surface(width: Int, height: Int): dom.HTMLCanvasElement =
    val canvas =
      dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
    canvas.width = width
    canvas.height = height
    canvas

  private def context(
      surface: dom.HTMLCanvasElement
  ): dom.CanvasRenderingContext2D =
    surface.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  private[p5] def capture(source: dom.HTMLCanvasElement): P5Image =
    copy(source.width, source.height)(_.drawImage(source, 0.0, 0.0))

  private def copy(width: Int, height: Int)(
      draw: dom.CanvasRenderingContext2D => Unit
  ): P5Image =
    require(width > 0 && height > 0, "image dimensions must be positive")
    val captured = surface(width, height)
    val drawing = context(captured)
    draw(drawing)
    new P5Image(
      captured,
      drawing.getImageData(0.0, 0.0, width, height)
    )

  private[p5] def upload(source: Image): dom.HTMLCanvasElement =
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

@js.native
private[p5] trait BitmapDrawingContext extends js.Object:
  def drawImage(source: dom.ImageBitmap, x: Double, y: Double): Unit = js.native
