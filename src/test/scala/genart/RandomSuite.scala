package genart

import genart.lib.random.{Rng, Seed}

class RandomSuite extends munit.FunSuite:
  test("seed serialization and streams match reference fixtures") {
    val fixtures = List(
      (
        "0000000000000000",
        List(
          "e220a8397b1dcdaf",
          "6e789e6aa1b965f4",
          "06c45d188009454f",
          "f88bb8a8724c81ec"
        ),
        "0ec10f8d906a6978",
        "de347074d432bb05",
        "c0c7e75614897c5f"
      ),
      (
        "ffffffffffffffff",
        List(
          "e4d971771b652c20",
          "e99ff867dbf682c9",
          "382ff84cb27281e9",
          "6d1db36ccba982d2"
        ),
        "805493391085b8f5",
        "28d515bc654bddd4",
        "3f3a2dbd645eafcb"
      ),
      (
        "8000000000000000",
        List(
          "481ec0a212a9f3db",
          "c46fa638a6309012",
          "61a685ffc80a8140",
          "592e268383e356f9"
        ),
        "3c696c1ff8745dae",
        "8bd59e96b1ad4375",
        "8f9a34e4bfc5b9f1"
      ),
      (
        "0020000000000001",
        List(
          "055ce0bfd3337037",
          "b5fbd32206751562",
          "4ba6a57bc047287b",
          "9eee1b40c84bfc57"
        ),
        "ee2ffd42c751239d",
        "2993fd4fccb472b8",
        "2b95c67e39ef5eb9"
      ),
      (
        "0123456789abcdef",
        List(
          "157a3807a48faa9d",
          "d573529b34a1d093",
          "2f90b72e996dccbe",
          "a2d419334c4667ec"
        ),
        "9f2cdd45438241d0",
        "3bc9bec4e481eecf",
        "32108e773b5cf971"
      )
    )
    for (hex, expected, palette, simulation, unicode) <- fixtures do
      val seed = Seed.fromHex(hex)
      assert(seed.toHex == hex)
      assert(Seed.fromHex(hex.toUpperCase) == seed)
      assert(Seed.fromHex(seed.toHex).hashCode() == seed.hashCode())
      assert(Set(seed).contains(Seed.fromHex(hex)))
      val rng = Rng(seed)
      assert(longs(rng, expected.size).map(Seed.fromLong(_).toHex) == expected)
      val (_, next) = rng.nextLong
      assert(next != rng)
      assert(seed.derive("palette").toHex == palette)
      assert(seed.derive("simulation").toHex == simulation)
      assert(seed.derive("café🎨").toHex == unicode)
  }

  test("seed derivation handles full-width values and UTF-8 boundaries") {
    assert(Seed.fromLong(0L).toHex == "0000000000000000")
    assert(Seed.fromLong(-1L).toHex == "ffffffffffffffff")
    assert(Seed.fromLong(Long.MinValue).toHex == "8000000000000000")
    assert(Seed.fromLong(9007199254740993L).toHex == "0020000000000001")
    val zero = Seed.fromLong(0L)
    assert(zero.derive("").toHex == "813f0174a2367c13")
    val utf8Boundaries =
      "\u007f\u0080\u07ff\u0800\ud7ff\ue000\uffff\ud800\udc00\udbff\udfff"
    assert(zero.derive(utf8Boundaries).toHex == "e227790d96d9101a")
    assert(zero.derive("\u0000") != zero.derive(""))
    for label <- List("\ud800", "\udfff", "\ud800x", "\ud800\ud800") do
      assertRejected(zero.derive(label))
  }

  test("seed parsing rejects invalid hexadecimal input") {
    for hex <- List(
        "",
        "0",
        "00000000000000000",
        "0x00000000000000",
        "gggggggggggggggg",
        " 000000000000000"
      )
    do assertRejected(Seed.fromHex(hex))
  }

  test("named streams are reproducible and independent") {
    val root = Seed.fromLong(42L)
    val simulation = Rng(root.derive("simulation"))
    val replay = Rng(root.derive("simulation"))
    val palette = Rng(root.derive("palette"))
    assert(longs(palette, 1000) != longs(simulation, 1000))
    assert(longs(simulation, 1000) == longs(replay, 1000))

  }

  test(
    "double sampling matches its fixture and bounded integers stay in range"
  ) {
    val rng = Rng(Seed.fromLong(0L))
    assert(rng.nextDouble._1 == 0.8833108082136426)
    assert(rng.nextDouble._2 == rng.nextLong._2)
    for bound <- List(1, 2, 3, 256, 1000000007, Int.MaxValue) do
      Iterator
        .unfold(rng)(state => Some(state.nextInt(bound)))
        .take(1000)
        .foreach { value =>
          assert(value >= 0 && value < bound)
        }

  }

  test("bounded integer rejection consumes another draw") {
    val rejectionSeed = Seed.fromHex("31628af67b2131ab")
    val rejectionRng = Rng(rejectionSeed)
    assert(rejectionRng.nextLong._1 == -1L)
    val (bounded, afterRejection) = rejectionRng.nextInt(Int.MaxValue)
    assert(bounded == 171447430)
    assert(
      Seed.fromLong(afterRejection.nextLong._1).toHex == "cdfa10a2e2ff33d6"
    )
    assert(afterRejection == rejectionRng.nextLong._2.nextLong._2)

  }

  test("uniform sampling keeps its upper bound exclusive at numeric extremes") {
    val rng = Rng(Seed.fromLong(0L))
    val rejectionRng = Rng(Seed.fromHex("31628af67b2131ab"))
    val ranges = List(
      (0.0, 1.0),
      (-10.0, -2.0),
      (-2.0, 10.0),
      (-Double.MaxValue, Double.MaxValue),
      (1.0, math.nextUp(1.0)),
      (0.0, Double.MinPositiveValue),
      (-Double.MinPositiveValue, 0.0)
    )
    for (low, high) <- ranges do
      Iterator
        .unfold(rng)(state => Some(state.uniform(low, high)))
        .take(1000)
        .foreach { value =>
          assert(value >= low && value < high)
        }
      val (rounded, _) = rejectionRng.uniform(low, high)
      assert(rounded >= low && rounded < high)

  }

  test("invalid sampling ranges are rejected") {
    val root = Seed.fromLong(42L)
    val untouched = Rng(root)
    for bound <- List(0, -1, Int.MinValue) do
      assertRejected(untouched.nextInt(bound))
    for (low, high) <- List(
        (0.0, 0.0),
        (1.0, 0.0),
        (Double.NaN, 1.0),
        (0.0, Double.NaN),
        (Double.NegativeInfinity, 1.0),
        (0.0, Double.PositiveInfinity)
      )
    do assertRejected(untouched.uniform(low, high))
  }

  private def longs(rng: Rng, count: Int): Vector[Long] =
    Iterator.unfold(rng)(state => Some(state.nextLong)).take(count).toVector

  private def assertRejected(body: => Any): Unit =
    intercept[IllegalArgumentException](body)
