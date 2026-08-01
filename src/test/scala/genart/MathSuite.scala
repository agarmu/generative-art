package genart

import genart.lib.math.{Vec, clamp, lerp}

class MathSuite extends munit.FunSuite:
  test("clamp handles boundaries, infinities and NaN") {
    assert(clamp(-2.0, 0.0, 1.0) == 0.0)
    assert(clamp(2.0, 0.0, 1.0) == 1.0)
    assert(clamp(0.25, 0.0, 1.0) == 0.25)
    assert(clamp(0.0, 0.0, 0.0) == 0.0)
    assert(clamp(Double.NegativeInfinity, 0.0, 1.0) == 0.0)
    assert(clamp(Double.PositiveInfinity, 0.0, 1.0) == 1.0)
    assert(clamp(Double.NaN, 0.0, 1.0).isNaN)
    assert(clamp(-2f, 0f, 1f) == 0f)
    assert(clamp(2f, 0f, 1f) == 1f)
    assert(clamp(0.25f, 0f, 1f) == 0.25f)
    for (lower, upper) <- List((1.0, 0.0), (Double.NaN, 1.0)) do
      assert(
        scala.util
          .Try(clamp(0.5, lower, upper))
          .failed
          .toOption
          .exists(_.isInstanceOf[IllegalArgumentException])
      )

  }

  test("vector norms use all components") {
    val vector = Vec(-3.0, 4.0)
    assert(vector.l1 == 7.0)
    assert(vector.l2 == 5.0)
    assert(vector.length == vector.l2)
    assert(Vec.Zero[4].l1 == 0.0)
    assert(Vec(-2.0).l1 == 2.0)

  }

  test("normalization handles zero and finite extremes") {
    for value <- List(Double.MaxValue, Double.MinPositiveValue, 1e-300) do
      val normalized = Vec(value, value).normalized
      assert(math.abs(normalized.length - 1.0) < 1e-12)
      assert(math.abs(normalized.x - 1.0 / math.sqrt(2.0)) < 1e-12)
    assert(Vec.Zero[3].normalized == Vec.Zero[3])
    for value <- List(Double.NaN, Double.PositiveInfinity) do
      assert(scala.util.Try(Vec(value).normalized).isFailure)

  }

  test("scalar interpolation supports extrapolation and finite extremes") {
    assert(lerp(10.0, 20.0, 0.25) == 12.5)
    assert(lerp(20.0, 10.0, 0.25) == 17.5)
    assert(lerp(10.0, 20.0, -0.5) == 5.0)
    assert(lerp(10.0, 20.0, 1.5) == 25.0)
    assert(lerp(-Double.MaxValue, Double.MaxValue, 0.0) == -Double.MaxValue)
    assert(lerp(-Double.MaxValue, Double.MaxValue, 1.0) == Double.MaxValue)
    assert(lerp(-Double.MaxValue, Double.MaxValue, 0.5) == 0.0)

  }

  test("vector interpolation preserves dimensions and supports extrapolation") {
    val from = Vec(0.0, 10.0)
    val to = Vec(10.0, 30.0)
    assert(lerp(from, to, 0.5) == Vec(5.0, 20.0))
    assert(lerp(from, to, -1.0) == Vec(-10.0, -10.0))
    assert(lerp(from, to, 2.0) == Vec(20.0, 50.0))
    assert(lerp(Vec(0.0), Vec(4.0), 0.5) == Vec(2.0))
    assert(lerp(Vec.Zero[3], Vec(2.0, 4.0, 6.0), 0.5) == Vec(1.0, 2.0, 3.0))
    assert(
      lerp(Vec.Zero[4], Vec(2.0, 4.0, 6.0, 8.0), 0.5) ==
        Vec(1.0, 2.0, 3.0, 4.0)
    )
    assert(
      scala.compiletime.testing.typeChecks(
        """import genart.lib.math.{Vec, lerp}
         lerp(Vec(1.0), Vec(1.0, 2.0), 0.5)
      """
      ) == false
    )
  }
