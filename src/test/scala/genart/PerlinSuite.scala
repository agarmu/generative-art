package genart

import genart.lib.math.Vec
import genart.lib.noise.PerlinNoise
import genart.lib.random.Seed
import scala.compiletime.testing.typeChecks

class PerlinSuite extends munit.FunSuite:
  private val fixtures = List(
    (
      "0000000000000000",
      Vector(
        0.445526123046875,
        0.56267243623733521,
        0.647883507313054,
        0.47148971003707629
      ),
      Vector(
        0.70775592465490655,
        0.65068945452401794,
        0.55127435026919047,
        0.4247796603723169
      )
    ),
    (
      "ffffffffffffffff",
      Vector(
        0.56851959228515625,
        0.39727985113859177,
        0.30859606993317357,
        0.4711841742585765
      ),
      Vector(
        0.44122878920937514,
        0.67901752736026522,
        0.49806968012609609,
        0.41008395823933214
      )
    ),
    (
      "8000000000000000",
      Vector(
        0.56851959228515625,
        0.48053857684135437,
        0.47944774131519807,
        0.63103663723509573
      ),
      Vector(
        0.29224407534509345,
        0.44923448959539947,
        0.48349974263333406,
        0.21609624038426489
      )
    ),
    (
      "0020000000000001",
      Vector(
        0.554473876953125,
        0.4054981991648674,
        0.51121759746592943,
        0.67888599529116167
      ),
      Vector(
        0.70775592465490655,
        0.63290759518602924,
        0.74033462002982853,
        0.54356713257644884
      )
    ),
    (
      "0123456789abcdef",
      Vector(
        0.445526123046875,
        0.38084660470485687,
        0.5492606084203544,
        0.53769403416129258
      ),
      Vector(
        0.70775592465490655,
        0.39863434153882804,
        0.42154890442245369,
        0.69092623478843895
      )
    )
  )
  for (hex, first, second) <- fixtures do
    val seed = Seed.fromHex(hex)
    val noise1 = PerlinNoise[1](seed)
    val noise2 = PerlinNoise[2](seed)
    val noise3 = PerlinNoise[3](seed)
    val noise4 = PerlinNoise[4](seed)
    def sample(x: Double, y: Double, z: Double, w: Double): Vector[Double] =
      Vector(
        noise1(Vec(x)),
        noise2(Vec(x, y)),
        noise3(Vec(x, y, z)),
        noise4(Vec(x, y, z, w))
      )

    test(s"noise matches reference values: $hex") {
      for
        (actual, expected) <- List(
          sample(0.125, 0.75, -0.375, 1.25) -> first,
          sample(-12.345, -6.789, 0.456, 9.123) -> second
        );
        (value, reference) <- actual.zip(expected)
      do assertEqualsDouble(value, reference, 1e-12)
    }

    test(s"noise is continuous at integer lattice points: $hex") {
      for x <- -3 to 3; y <- -3 to 3 do
        assert(sample(x, y, 0.0, 0.0).forall(_ == 0.5))
        for epsilon <- List(-1e-8, 1e-8) do
          assert(
            sample(x + epsilon, y - epsilon, epsilon, -epsilon)
              .forall(value => math.abs(value - 0.5) < 1e-6)
          )
    }

    test(s"noise remains normalized: $hex") {
      for i <- 0 until 1000 do
        val values = sample(i * 0.137, -i * 0.293, i * 0.419, -i * 0.571)
        assert(values.forall(value => value >= 0.0 && value <= 1.0))
    }

    test(s"coordinate limits are enforced in every dimension: $hex") {
      for limit <- List(-PerlinNoise.MaxCoordinate, PerlinNoise.MaxCoordinate)
      do assert(sample(limit, limit, limit, limit).forall(_ == 0.5))
      for invalid <- List(
          Double.NaN,
          Double.PositiveInfinity,
          Double.NegativeInfinity,
          math.nextUp(PerlinNoise.MaxCoordinate),
          math.nextDown(-PerlinNoise.MaxCoordinate)
        )
      do
        assertRejected(noise1(Vec(invalid)))
        assertRejected(noise2(Vec(0.0, invalid)))
        assertRejected(noise3(Vec(0.0, 0.0, invalid)))
        assertRejected(noise4(Vec(0.0, 0.0, 0.0, invalid)))
    }

  test("seeded noise is reproducible and independent of evaluation order") {
    val seed = Seed.fromLong(42L)
    val point = Vec(0.125, 0.75)
    val noise = PerlinNoise[2](seed)
    val expected = noise(point)
    noise(Vec(-1.25, 6.5))
    assert(noise(point) == expected)
    assert(PerlinNoise[2](seed)(point) == expected)
    assert(PerlinNoise[2](Seed.fromLong(43L))(point) != expected)
  }

  test("unsupported dimensions and mismatched vectors fail to compile") {
    assert(
      !typeChecks(
        """new genart.lib.noise.PerlinNoise[5](genart.lib.random.Seed.fromLong(0L))"""
      )
    )
    assert(
      !typeChecks(
        """new genart.lib.noise.PerlinNoise[0](genart.lib.random.Seed.fromLong(0L))"""
      )
    )
    assert(
      !typeChecks(
        """genart.lib.noise.PerlinNoise[2](genart.lib.random.Seed.fromLong(0L))(genart.lib.math.Vec(1.0))"""
      )
    )
  }

  private def assertRejected(body: => Any): Unit =
    intercept[IllegalArgumentException](body)
