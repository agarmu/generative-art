package genart.lib.time

import genart.{Frame, Step}
import genart.lib.requireFinite

final case class ClockConfig(
    updatesPerSecond: Int = 60,
    maxUpdatesPerFrame: Int = 5,
    maxElapsedSeconds: Double = 0.25
):
  require(updatesPerSecond > 0, "updatesPerSecond must be positive")
  require(maxUpdatesPerFrame > 0, "maxUpdatesPerFrame must be positive")
  requireFinite(maxElapsedSeconds, "maxElapsedSeconds")
  require(maxElapsedSeconds > 0.0, "maxElapsedSeconds must be positive")
  requireFinite(maxElapsedSeconds * updatesPerSecond, "maximum elapsed ticks")
  val deltaSeconds: Double = 1.0 / updatesPerSecond

final case class ClockAdvance(
    next: FixedClock,
    steps: Vector[Step],
    frame: Frame,
    droppedSeconds: Double
)

final case class FixedClock private (
    config: ClockConfig,
    tick: Long,
    frameIndex: Long,
    private val fraction: Double
):
  require(tick >= 0L && frameIndex >= 0L, "clock indices must be nonnegative")
  require(fraction >= 0.0 && fraction < 1.0, "fraction must be in [0, 1)")

  def advance(elapsedSeconds: Double): ClockAdvance =
    requireFinite(elapsedSeconds, "elapsedSeconds")
    require(elapsedSeconds >= 0.0, "elapsedSeconds must be nonnegative")
    val elapsed = math.min(elapsedSeconds, config.maxElapsedSeconds)
    val accumulated = fraction + elapsed * config.updatesPerSecond
    val whole = math.floor(accumulated)
    val count = math.min(whole, config.maxUpdatesPerFrame.toDouble).toInt
    val remainder = accumulated - whole
    val dropped =
      elapsedSeconds - elapsed + (whole - count) * config.deltaSeconds
    result(count, elapsed, remainder, dropped)

  def replay(ticks: Int): ClockAdvance =
    require(ticks >= 0, "ticks must be nonnegative")
    result(ticks, ticks * config.deltaSeconds, 0.0, 0.0)

  private def result(
      count: Int,
      elapsed: Double,
      remainder: Double,
      dropped: Double
  ): ClockAdvance =
    require(tick <= Long.MaxValue - count, "tick index exhausted")
    require(frameIndex < Long.MaxValue, "frame index exhausted")
    val steps = Vector.tabulate(count) { offset =>
      val index = tick + offset + 1L
      Step(index, index * config.deltaSeconds, config.deltaSeconds)
    }
    ClockAdvance(
      copy(
        tick = tick + count,
        frameIndex = frameIndex + 1L,
        fraction = remainder
      ),
      steps,
      Frame(frameIndex, elapsed, remainder),
      dropped
    )

object FixedClock:
  def apply(config: ClockConfig = ClockConfig()): FixedClock =
    new FixedClock(config, 0L, 0L, 0.0)
