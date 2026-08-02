package genart.lib.noise

import genart.lib.requireFinite
import genart.lib.math.{Vec, clamp}
import genart.lib.random.{Rng, Seed}
import scala.annotation.tailrec

final class PerlinNoise[N <: 1 | 2 | 3 | 4](seed: Seed)(using
    dimension: ValueOf[N]
):
  private val dimensions = dimension.value
  private val permutation =
    val (values, _) =
      (255 to 1 by -1).foldLeft((Vector.range(0, 256), Rng(seed))) {
        case ((values, rng), index) =>
          val (other, next) = rng.nextInt(index + 1)
          (
            values.updated(index, values(other)).updated(other, values(index)),
            next
          )
      }
    values

  private val gradients =
    if dimensions == 1 then Vector(Vector(-1.0), Vector(1.0))
    else
      val signs = (1 until dimensions).foldLeft(Vector(Vector.empty[Double])) {
        (prefixes, _) =>
          for prefix <- prefixes; sign <- Vector(-1.0, 1.0) yield prefix :+ sign
      }
      (0 until dimensions).toVector.flatMap { axis =>
        signs.map(_.patch(axis, Vector(0.0), 0))
      }

  def apply(point: Vec[N]): Double =
    val coordinates = point.toVector
    coordinates.foreach { value =>
      requireFinite(value, "coordinate")
      require(
        value.abs <= PerlinNoise.MaxCoordinate,
        "noise coordinate exceeds supported range"
      )
    }
    clamp(0.5 + 0.5 * perlin(coordinates), 0.0, 1.0)

  private def perlin(coordinates: Vector[Double]): Double =
    val axes = coordinates.map { coordinate =>
      val floor = math.floor(coordinate)
      val fraction = coordinate - floor
      PerlinNoise.Axis(floor.toInt, fraction, fade(fraction))
    }

    def corner(axis: Int, hash: Int, mask: Int): Double =
      if axis == dimensions then
        val gradient = gradients(hash % gradients.size)
        @tailrec def dot(index: Int, sum: Double): Double =
          if index == dimensions then sum
          else
            val offset = axes(index).fraction - ((mask >>> index) & 1)
            dot(index + 1, sum + gradient(index) * offset)
        dot(0, 0.0)
      else
        val cell = axes(axis).cell
        val left = corner(axis + 1, permutation((hash + cell) & 255), mask)
        val right = corner(
          axis + 1,
          permutation((hash + cell + 1) & 255),
          mask | (1 << axis)
        )
        left + axes(axis).fade * (right - left)
    corner(0, 0, 0)

  private def fade(value: Double): Double =
    value * value * value * (value * (value * 6.0 - 15.0) + 10.0)

object PerlinNoise:
  val MaxCoordinate: Double = 67108864.0

  private case class Axis(cell: Int, fraction: Double, fade: Double)
