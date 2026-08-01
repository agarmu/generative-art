package genart.lib.math

import genart.lib.requireFinite

final class Vec[N <: 1 | 2 | 3 | 4] private[math] (
    private[math] val components: Vector[Double]
):
  def toVector: Vector[Double] = components

  def x: Double = components(0)
  def y(using N <:< (2 | 3 | 4)): Double = components(1)
  def z(using N <:< (3 | 4)): Double = components(2)
  def w(using N =:= 4): Double = components(3)

  def +(other: Vec[N]): Vec[N] =
    new Vec(components.zip(other.components).map(_ + _))

  def -(other: Vec[N]): Vec[N] =
    new Vec(components.zip(other.components).map(_ - _))

  def *(scalar: Double): Vec[N] =
    new Vec(components.map(_ * scalar))

  def dot(other: Vec[N]): Double =
    components.zip(other.components).map(_ * _).sum

  def l1: Double = components.foldLeft(0.0)((sum, value) => sum + value.abs)

  def l2: Double = components.foldLeft(0.0)(scala.math.hypot)

  def length: Double = l2

  def normalized: Vec[N] =
    components.foreach(value => requireFinite(value, "component"))
    val scale = components.iterator.map(_.abs).max
    if scale == 0.0 then this
    else
      val scaled = components.map(_ / scale)
      val magnitude = scaled.foldLeft(0.0)(scala.math.hypot)
      new Vec(scaled.map(_ / magnitude))

  override def equals(other: Any): Boolean = other match
    case that: Vec[?] => components == that.components
    case _            => false

  override def hashCode(): Int = components.hashCode()
  override def toString: String =
    components.mkString(s"Vec[${components.size}](", ", ", ")")

object Vec:
  def apply(x: Double): Vec[1] =
    new Vec(Vector(x))

  def apply(x: Double, y: Double): Vec[2] =
    new Vec(Vector(x, y))

  def apply(x: Double, y: Double, z: Double): Vec[3] =
    new Vec(Vector(x, y, z))

  def apply(x: Double, y: Double, z: Double, w: Double): Vec[4] =
    new Vec(Vector(x, y, z, w))

  def Zero[N <: 1 | 2 | 3 | 4](using dimension: ValueOf[N]): Vec[N] =
    new Vec(Vector.fill(dimension.value)(0.0))
