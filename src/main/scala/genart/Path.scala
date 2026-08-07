package genart

import genart.lib.requireFinite
import genart.lib.math.{Vec, floorMod}

enum ArcMode:
  case Open, Chord, Pie

final case class Path(
    start: Vec[2],
    segments: Vector[Path.Segment] = Vector.empty,
    closed: Boolean = false
):
  def lineTo(to: Vec[2]): Path =
    copy(segments = segments :+ Path.Segment.Line(to))

  def quadraticTo(control: Vec[2], to: Vec[2]): Path =
    copy(segments = segments :+ Path.Segment.Quadratic(control, to))

  def bezierTo(control1: Vec[2], control2: Vec[2], to: Vec[2]): Path =
    copy(segments = segments :+ Path.Segment.Cubic(control1, control2, to))

  def arcTo(
      center: Vec[2],
      size: Vec[2],
      startAngle: Double,
      sweepAngle: Double
  ): Path = copy(segments =
    segments :+
      Path.Segment.Arc(center, size, startAngle, sweepAngle)
  )

  def close: Path = copy(closed = true)

object Path:
  enum Segment:
    case Line(to: Vec[2])
    case Quadratic(control: Vec[2], to: Vec[2])
    case Cubic(control1: Vec[2], control2: Vec[2], to: Vec[2])
    case Arc(
        center: Vec[2],
        size: Vec[2],
        startAngle: Double,
        sweepAngle: Double
    )

  def arc(
      center: Vec[2],
      size: Vec[2],
      startAngle: Double,
      sweepAngle: Double,
      mode: ArcMode = ArcMode.Open
  ): Path =
    validateArc(size, startAngle, sweepAngle)
    val start = ellipsePoint(center, size, startAngle)
    if sweepAngle == 0.0 || size.x == 0.0 || size.y == 0.0 then Path(start)
    else
      val outline = Path(start).arcTo(center, size, startAngle, sweepAngle)
      if sweepAngle == 2.0 * math.Pi then outline.close
      else
        mode match
          case ArcMode.Open  => outline
          case ArcMode.Chord => outline.close
          case ArcMode.Pie   =>
            Path(center).arcTo(center, size, startAngle, sweepAngle).close

  private[genart] def normalizedAngle(angle: Double): Double =
    floorMod(angle, 2.0 * math.Pi)

  private[genart] def ellipsePoint(
      center: Vec[2],
      size: Vec[2],
      angle: Double
  ): Vec[2] =
    val normalized = normalizedAngle(angle)
    Vec(
      center.x + size.x / 2.0 * math.cos(normalized),
      center.y + size.y / 2.0 * math.sin(normalized)
    )

  private[genart] def validateArc(
      size: Vec[2],
      start: Double,
      sweep: Double
  ): Unit =
    requireFinite(start, "startAngle")
    requireFinite(sweep, "sweepAngle")
    requireFinite(size.x, "width")
    requireFinite(size.y, "height")
    require(size.x >= 0.0 && size.y >= 0.0, "dimensions must be nonnegative")
    require(
      sweep >= 0.0 && sweep <= 2.0 * math.Pi,
      "sweepAngle must be in [0, 2π]"
    )

  private[genart] def points(path: Path): Iterator[Vec[2]] =
    Iterator.single(path.start) ++ path.segments.iterator.flatMap {
      case Segment.Line(to)                      => Iterator.single(to)
      case Segment.Quadratic(control, to)        => Iterator(control, to)
      case Segment.Cubic(control1, control2, to) =>
        Iterator(control1, control2, to)
      case Segment.Arc(center, size, _, _) => Iterator(center, size)
    }
