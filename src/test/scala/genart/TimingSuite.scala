package genart

import genart.lib.time.{ClockConfig, FixedClock}

class TimingSuite extends munit.FunSuite:
  private val clock = FixedClock()

  test("fixed steps derive time from ticks without mutating the clock") {
    val empty = clock.advance(0.0)
    assert(empty.steps.isEmpty)
    assert(empty.frame == Frame(0L, 0.0, 0.0))
    val one = clock.advance(1.0 / 60.0)
    assert(one.steps == Vector(Step(1L, 1.0 / 60.0, 1.0 / 60.0)))
    assert(one.next.tick == 1L && one.next.frameIndex == 1L)
    assert(clock.tick == 0L)
  }

  test("fractional elapsed time accumulates across frames") {
    val half = clock.advance(0.5 / 60.0)
    assert(half.steps.isEmpty && half.frame.interpolation == 0.5)
    val completed = half.next.advance(0.5 / 60.0)
    assert(completed.steps == clock.advance(1.0 / 60.0).steps)
    assert(completed.frame.interpolation == 0.0)

  }

  test("catch-up caps discard whole-step debt while preserving interpolation") {
    val capped = clock.advance(20.5 / 60.0)
    assert(capped.steps.size == 5)
    assert(math.abs(capped.droppedSeconds - (20.5 / 60.0 - 5.0 / 60.0)) < 1e-12)
    assert(capped.frame.deltaSeconds == 0.25)
    assert(capped.next.advance(0.0).steps.isEmpty)
    val fractional = clock.advance(10.5 / 60.0)
    assert(fractional.steps.size == 5)
    assert(math.abs(fractional.frame.interpolation - 0.5) < 1e-12)
    assert(math.abs(fractional.droppedSeconds - 5.0 / 60.0) < 1e-12)

  }

  test(
    "different render schedules and replay produce identical simulation steps"
  ) {
    def run(schedule: Vector[Double]): (FixedClock, Vector[Step]) =
      schedule.foldLeft((clock, Vector.empty[Step])) {
        case ((current, steps), elapsed) =>
          val result = current.advance(elapsed)
          (result.next, steps ++ result.steps)
      }
    val (slow, slowSteps) = run(Vector.fill(20)(3.0 / 60.0))
    val (fast, fastSteps) = run(Vector.fill(120)(0.5 / 60.0))
    assert(slow.tick == 60L && fast.tick == 60L)
    assert(slowSteps == fastSteps)
    val replay = clock.replay(60)
    assert(replay.steps == slowSteps)
    assert(replay.droppedSeconds == 0.0)
    assert(replay.frame.interpolation == 0.0)
    assert(
      replay.next
        .replay(1)
        .steps
        .head
        .seconds == 61L * clock.config.deltaSeconds
    )

  }

  test(
    "invalid elapsed times, replay counts and clock configurations are rejected"
  ) {
    for elapsed <- List(-1.0, Double.NaN, Double.PositiveInfinity) do
      assertRejected(clock.advance(elapsed))
    assertRejected(clock.replay(-1))
    assertRejected(ClockConfig(updatesPerSecond = 0))
    assertRejected(ClockConfig(maxUpdatesPerFrame = 0))
    assertRejected(ClockConfig(maxElapsedSeconds = Double.NaN))
    assertRejected(ClockConfig(maxElapsedSeconds = Double.MaxValue))
  }

  private def assertRejected(body: => Any): Unit =
    intercept[IllegalArgumentException](body)
