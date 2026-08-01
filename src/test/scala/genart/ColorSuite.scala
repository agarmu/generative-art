package genart

import genart.lib.math.floorMod

class ColorSuite extends munit.FunSuite:
  test("equal colors work as hash collection keys, including signed zero") {
    val color = Color.rgb(0.25, 0.5, 0.75, 0.8)
    val equalColor = Color.rgb(0.25, 0.5, 0.75, 0.8)
    assert(color == equalColor)
    assert(color.hashCode() == equalColor.hashCode())
    assert(Set(color).contains(equalColor))
    assert(Map(color -> "stored").get(equalColor).contains("stored"))

    val positiveZero = Color.rgb(0.0, 0.0, 0.0, 0.0)
    val negativeZero = Color.rgb(-0.0, -0.0, -0.0, -0.0)
    assert(positiveZero == negativeZero)
    assert(positiveZero.hashCode() == negativeZero.hashCode())
    assert(Set(positiveZero).contains(negativeZero))

  }

  test("hue wraps across positive and negative turns") {
    assert(floorMod(-30.0, 360.0) == 330.0)
    assert(floorMod(390.0, 360.0) == 30.0)
    assert(floorMod(-720.0, 360.0) == 0.0)
    val nearBoundary = floorMod(-Double.MinPositiveValue, 360.0)
    assert(nearBoundary >= 0.0 && nearBoundary < 360.0)
    assert(Color.hsb(-120.0, 1.0, 1.0) == Color.rgb(0.0, 0.0, 1.0))
  }

  test("HSB conversion covers all six hue sectors") {
    val sectorMidpoints = List(
      Color.rgb(0.8, 0.6, 0.4, 0.5),
      Color.rgb(0.6, 0.8, 0.4, 0.5),
      Color.rgb(0.4, 0.8, 0.6, 0.5),
      Color.rgb(0.4, 0.6, 0.8, 0.5),
      Color.rgb(0.6, 0.4, 0.8, 0.5),
      Color.rgb(0.8, 0.4, 0.6, 0.5)
    )
    for (expected, sector) <- sectorMidpoints.zipWithIndex do
      val actual = Color.hsb(30.0 + sector * 60.0, 0.5, 0.8, 0.5)
      assert(math.abs(actual.red - expected.red) < 1e-12)
      assert(math.abs(actual.green - expected.green) < 1e-12)
      assert(math.abs(actual.blue - expected.blue) < 1e-12)
      assert(actual.alpha == expected.alpha)

  }

  test("mix uses premultiplied alpha and preserves endpoint values") {
    val hiddenRed = Color.rgb(1.0, 0.0, 0.0, 0.0)
    val blue = Color.rgb(0.0, 0.0, 1.0)
    assertEquals(hiddenRed.mix(blue, 0.0), hiddenRed)
    assertEquals(blue.mix(hiddenRed, 1.0), hiddenRed)
    assert(hiddenRed.mix(blue, 0.5) == blue.withAlpha(0.5))
    assert(
      hiddenRed.mix(hiddenRed, 0.5) == Color.rgb(0.0, 0.0, 0.0, 0.0)
    )

    val red = Color.rgb(1.0, 0.0, 0.0)
    assertEquals(red.mix(blue, 0.5), Color.rgb(0.5, 0.0, 0.5))
    assertEquals(red.mix(blue, 0.25), Color.rgb(0.75, 0.0, 0.25))
    assertEquals(
      red.withAlpha(0.25).mix(blue.withAlpha(0.75), 0.5),
      Color.rgb(0.25, 0.0, 0.75, 0.5)
    )
  }

  test("mix handles subnormal alpha and underflow") {
    val hiddenRed = Color.rgb(1.0, 0.0, 0.0, 0.0)
    val tinyGray = Color.rgb(0.5, 0.5, 0.5, Double.MinPositiveValue)
    assert(tinyGray.mix(tinyGray, 0.5) == tinyGray)
    val tinyRed = Color.rgb(1.0, 0.0, 0.0, Double.MinPositiveValue)
    assert(tinyRed.mix(hiddenRed, 0.5) == Color.rgb(0.0, 0.0, 0.0, 0.0))
    assert(hiddenRed.mix(tinyRed, 0.5) == Color.rgb(0.0, 0.0, 0.0, 0.0))
  }

  test("style rejects invalid stroke weights") {
    for weight <- List(-1.0, Double.NaN, Double.PositiveInfinity) do
      intercept[IllegalArgumentException](Style(None, None, weight))
    assert(Style(None, None, 0.0).strokeWeight == 0.0)

  }

  test(
    "color components and interpolation amounts must be finite unit values"
  ) {
    val red = Color.rgb(1.0, 0.0, 0.0)
    val blue = Color.rgb(0.0, 0.0, 1.0)
    for invalid <- List(-0.1, 1.1, Double.NaN, Double.PositiveInfinity) do
      val rejected = List(
        () => Color.rgb(invalid, 0.0, 0.0),
        () => Color.rgb(0.0, 0.0, 0.0, invalid),
        () => red.withAlpha(invalid),
        () => red.mix(blue, invalid),
        () => Color.hsb(0.0, invalid, 1.0)
      )
      rejected.foreach(action => intercept[IllegalArgumentException](action()))
  }
