package genart.lib.math

import genart.lib.requireFinite

/** Restrict a value to the inclusive bounds; NaN values remain NaN. */
def clamp(value: Double, lower: Double, upper: Double): Double =
  require(lower <= upper, "lower bound must not exceed upper bound")
  scala.math.max(lower, scala.math.min(value, upper))

def clamp(value: Float, lower: Float, upper: Float): Float =
  clamp(value.toDouble, lower.toDouble, upper.toDouble).toFloat

/** Linear interpolation, with extrapolation outside [0, 1]. */
def lerp(from: Double, to: Double, amount: Double): Double =
  if amount == 0.0 then from
  else if amount == 1.0 then to
  else if amount > 0.0 && amount < 1.0 then (1.0 - amount) * from + amount * to
  else from + amount * (to - from)

/** Floating-point floor modulus in [0, modulus). Requires a finite value and a
  * finite, positive modulus.
  */
def floorMod(value: Double, modulus: Double): Double =
  requireFinite(value)
  require(
    modulus > 0.0 && !modulus.isInfinity,
    "modulus must be finite and positive"
  )
  val remainder = value % modulus
  if remainder == 0.0 then 0.0
  else if remainder < 0.0 then
    // Addition can round up to modulus for very small negative remainders.
    scala.math.min(remainder + modulus, scala.math.nextDown(modulus))
  else remainder

/** Component-wise interpolation between vectors of the same dimension. */
def lerp[N <: 1 | 2 | 3 | 4](
    from: Vec[N],
    to: Vec[N],
    amount: Double
): Vec[N] =
  if amount == 0.0 then from
  else if amount == 1.0 then to
  else
    new Vec(from.components.zip(to.components).map { (left, right) =>
      lerp(left, right, amount)
    })
