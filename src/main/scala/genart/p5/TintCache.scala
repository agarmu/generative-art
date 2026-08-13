package genart.p5

import genart.{Color, Image}
import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("WeakMap")
private[p5] class TintEntries extends js.Object:
  def get(source: Image): js.UndefOr[Vector[(Color, dom.HTMLCanvasElement)]] =
    js.native
  def set(
      source: Image,
      variants: Vector[(Color, dom.HTMLCanvasElement)]
  ): TintEntries = js.native

/** Weakly caches up to four tint variants of each immutable source image. */
private[p5] final class TintCache:
  private val entries = new TintEntries

  def getOrCreate(source: Image, tint: Color): dom.HTMLCanvasElement =
    val variants = entries.get(source).getOrElse(Vector.empty)
    variants.find(_._1 == tint).map(_._2).getOrElse {
      // Images already retain CPU pixels; reuse the native upload conversion.
      val surface = P5Image.upload(source, tint)
      entries.set(source, variants.takeRight(3) :+ (tint, surface))
      surface
    }
