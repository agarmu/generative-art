package genart.processing

import genart.{Color, Image}
import java.awt.image.BufferedImage

private[processing] final class ProcessingImage(val bitmap: BufferedImage)
    extends Image(bitmap.getWidth, bitmap.getHeight):
  protected def read(x: Int, y: Int): Color =
    val argb = bitmap.getRGB(x, y)
    Color.rgb(
      ((argb >>> 16) & 255) / 255.0,
      ((argb >>> 8) & 255) / 255.0,
      (argb & 255) / 255.0,
      ((argb >>> 24) & 255) / 255.0
    )

private[processing] object ProcessingImage:
  def upload(source: Image): BufferedImage =
    val bitmap = new BufferedImage(
      source.width,
      source.height,
      BufferedImage.TYPE_INT_ARGB
    )
    val pixels = Array.tabulate(source.width * source.height) { index =>
      val color = source(index % source.width, index / source.width)
      def channel(value: Double): Int = math.round(value * 255.0).toInt
      (channel(color.alpha) << 24) | (channel(color.red) << 16) |
        (channel(color.green) << 8) | channel(color.blue)
    }
    bitmap.setRGB(0, 0, source.width, source.height, pixels, 0, source.width)
    bitmap
