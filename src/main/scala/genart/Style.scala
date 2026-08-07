package genart

import genart.lib.requireFinite

enum StrokeCap:
  case Butt, Round, Square

enum StrokeJoin:
  case Miter, Round, Bevel

final case class Style(
    stroke: Option[Color],
    fill: Option[Color],
    strokeWeight: Double,
    strokeCap: StrokeCap = StrokeCap.Round,
    strokeJoin: StrokeJoin = StrokeJoin.Miter
):
  requireFinite(strokeWeight, "strokeWeight")
  require(strokeWeight >= 0.0, "strokeWeight must be nonnegative")
