package genart

import genart.lib.{requireFinite, requireUnit}
import genart.lib.math.Vec

type Paint = Color | Gradient

/** Gradient coordinates are local drawing coordinates, transformed with shapes.
  * RGBA channels interpolate separately in sRGB; end-stop colors are clamped.
  */
sealed trait Gradient:
  def stops: Vector[Gradient.Stop]

object Gradient:
  final case class Stop(offset: Double, color: Color):
    requireUnit(offset, "gradient stop offset")

  final case class Linear(
      from: Vec[2],
      to: Vec[2],
      stops: Vector[Stop]
  ) extends Gradient:
    validatePoint(from)
    validatePoint(to)
    require(from != to, "gradient endpoints must differ")
    validateStops(stops)

  final case class Radial(
      center: Vec[2],
      radius: Double,
      stops: Vector[Stop]
  ) extends Gradient:
    validatePoint(center)
    requireFinite(radius, "gradient radius")
    require(radius > 0.0, "gradient radius must be positive")
    validateStops(stops)

  private def validatePoint(point: Vec[2]): Unit =
    requireFinite(point.x, "gradient x")
    requireFinite(point.y, "gradient y")

  private def validateStops(stops: Vector[Stop]): Unit =
    require(stops.size >= 2, "a gradient needs at least two stops")
    require(
      stops.iterator.zip(stops.iterator.drop(1)).forall { (left, right) =>
        left.offset < right.offset
      },
      "gradient stops must be strictly increasing"
    )
