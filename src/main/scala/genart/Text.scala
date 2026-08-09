package genart

import genart.lib.requireFinite

enum FontFamily:
  case SansSerif, Serif, Monospace

enum TextAlign:
  case Left, Center, Right

enum TextBaseline:
  case Alphabetic, Top, Middle, Bottom

final case class TextStyle(
    size: Double = 16.0,
    family: FontFamily = FontFamily.SansSerif,
    bold: Boolean = false,
    italic: Boolean = false,
    align: TextAlign = TextAlign.Left,
    baseline: TextBaseline = TextBaseline.Alphabetic,
    lineHeight: Double = 1.2
):
  requireFinite(size, "text size")
  require(size > 0.0, "text size must be positive")
  requireFinite(lineHeight, "lineHeight")
  require(lineHeight > 0.0, "lineHeight must be positive")
  requireFinite(size * lineHeight, "line advance")

object TextStyle:
  val Default: TextStyle = TextStyle()

final case class TextMetrics(
    width: Double,
    height: Double,
    ascent: Double,
    descent: Double
)
