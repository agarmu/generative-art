package genart

import genart.lib.math.{floorMod, lerp}
import genart.lib.{requireFinite, requireUnit}

final class Color private (
    val red: Double,
    val green: Double,
    val blue: Double,
    val alpha: Double
):
  def withAlpha(value: Double): Color =
    requireUnit(value, "alpha")
    if value == alpha then this
    else new Color(red, green, blue, value)

  def mix(other: Color, amount: Double): Color =
    requireUnit(amount, "amount")
    if amount == 0.0 then this
    else if amount == 1.0 then other
    else
      val alphaScale = math.max(alpha, other.alpha)
      if alphaScale == 0.0 then Color.transparent
      else
        // Scale before weighting so subnormal alpha values retain precision.
        val fromAlpha = alpha / alphaScale
        val toAlpha = other.alpha / alphaScale
        val total = lerp(fromAlpha, toAlpha, amount)
        val mixedAlpha = alphaScale * total
        if mixedAlpha == 0.0 then Color.transparent
        else
          val rightWeight = amount * toAlpha / total
          new Color(
            lerp(red, other.red, rightWeight),
            lerp(green, other.green, rightWeight),
            lerp(blue, other.blue, rightWeight),
            mixedAlpha
          )

  override def equals(value: Any): Boolean = value match
    case other: Color =>
      red == other.red && green == other.green &&
      blue == other.blue && alpha == other.alpha
    case _ => false

  override def hashCode(): Int =
    (red, green, blue, alpha).hashCode()

  override def toString: String = s"Color($red, $green, $blue, $alpha)"

object Color:
  private val transparent = new Color(0.0, 0.0, 0.0, 0.0)

  def rgb(
      r: Double,
      g: Double,
      b: Double,
      alpha: Double = 1.0
  ): Color =
    requireUnit(r, "red")
    requireUnit(g, "green")
    requireUnit(b, "blue")
    requireUnit(alpha, "alpha")
    new Color(r, g, b, alpha)

  def hsb(
      hueDegrees: Double,
      saturation: Double,
      brightness: Double,
      alpha: Double = 1.0
  ): Color =
    requireFinite(hueDegrees, "hue")
    requireUnit(saturation, "saturation")
    requireUnit(brightness, "brightness")
    requireUnit(alpha, "alpha")
    val hue = floorMod(hueDegrees, 360.0) / 60.0
    val sector = math.floor(hue).toInt
    val fraction = hue - sector
    val p = brightness * (1.0 - saturation)
    val q = lerp(brightness, p, fraction)
    val t = lerp(p, brightness, fraction)
    (sector % 6) match
      case 0 => new Color(brightness, t, p, alpha)
      case 1 => new Color(q, brightness, p, alpha)
      case 2 => new Color(p, brightness, t, alpha)
      case 3 => new Color(p, q, brightness, alpha)
      case 4 => new Color(t, p, brightness, alpha)
      case _ => new Color(brightness, p, q, alpha)
