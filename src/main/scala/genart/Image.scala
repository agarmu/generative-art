package genart

import scala.concurrent.Future
import scala.util.Try

abstract class Image(final val width: Int, final val height: Int):
  require(width > 0 && height > 0, "image dimensions must be positive")

  final def apply(x: Int, y: Int): Color =
    if x < 0 || x >= width || y < 0 || y >= height then
      throw new IndexOutOfBoundsException(
        s"Point ($x, $y) outside ${width}x$height image"
      )
    read(x, y)

  protected def read(x: Int, y: Int): Color

trait ImageLoader:
  final def load(source: String): Future[Image] =
    Try {
      require(source.nonEmpty, "image source must not be empty")
      decode(source)
    }.fold(Future.failed, identity)

  protected def decode(source: String): Future[Image]
