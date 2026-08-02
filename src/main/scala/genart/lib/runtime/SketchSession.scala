package genart.lib.runtime

import genart.{Sketch, SketchContext, SketchProvider}
import genart.lib.requireFinite
import genart.lib.random.Seed
import genart.lib.time.{ClockAdvance, ClockConfig, FixedClock}
import scala.util.Using

final case class RunConfig(
    seed: Seed,
    clock: ClockConfig = ClockConfig(),
    frameRate: Double = 60.0
):
  requireFinite(frameRate, "frameRate")
  require(frameRate > 0.0, "frameRate must be positive")

final class SketchSession private (
    provider: SketchProvider,
    sketch: Sketch,
    initialClock: FixedClock
):
  import SketchSession.*

  private var state = State(initialClock)

  def isStopped: Boolean = state.stopped
  def isSuspended: Boolean = state.suspended

  def frame(nowSeconds: Double): Unit =
    if !isStopped && !isSuspended then
      dispatch {
        requireFinite(nowSeconds, "nowSeconds")
        state.lastTime.foreach { previous =>
          require(nowSeconds >= previous, "clock must be monotonic")
        }
        val elapsed = state.lastTime.fold(0.0) { previous =>
          val difference = nowSeconds - previous
          if difference.isInfinity then state.clock.config.maxElapsedSeconds
          else difference
        }
        val result = state.clock.advance(elapsed)
        state = state.copy(lastTime = Some(nowSeconds))
        render(result)
      }

  def replay(ticks: Int): Unit =
    if !isStopped then
      dispatch {
        val result = state.clock.replay(ticks)
        state = state.copy(lastTime = None)
        render(result)
      }

  def suspend(): Unit =
    if !isStopped then state = state.copy(suspended = true, lastTime = None)

  def resume(): Unit =
    if !isStopped && isSuspended then
      state = state.copy(suspended = false, lastTime = None)

  def resize(width: Int, height: Int): Unit =
    if !isStopped then
      dispatch {
        require(width > 0 && height > 0, "dimensions must be positive")
        provider.resize(width, height)
        require(
          provider.canvas.width == width && provider.canvas.height == height,
          "provider must update dimensions before the resize callback"
        )
        sketch.resized()
      }

  def stop(): Unit =
    if !isStopped then state = state.copy(stopped = true)
    if !state.dispatching then release()

  private def release(): Unit =
    if !state.closed then
      state = state.copy(closed = true)
      Using.resource(() => provider.close())(_ => sketch.dispose())(using
        releaseAction
      )

  private def render(result: ClockAdvance): Unit =
    state = state.copy(clock = result.next)
    result.steps.iterator.takeWhile(_ => !isStopped).foreach(sketch.update)
    if !isStopped then provider.beginFrame()
    if !isStopped then sketch.draw(result.frame)

  private def initialize(): Unit = dispatch(sketch.setup())

  private def dispatch(body: => Unit): Unit =
    require(!state.dispatching, "lifecycle callbacks cannot be reentered")
    state = state.copy(dispatching = true)
    Using.resource(() => {
      state = state.copy(dispatching = false)
      if isStopped then release()
    }) { _ =>
      try body
      catch
        case error: Throwable =>
          state = state.copy(stopped = true)
          throw error
    }(using releaseAction)

object SketchSession:
  private case class State(
      clock: FixedClock,
      lastTime: Option[Double] = None,
      suspended: Boolean = false,
      stopped: Boolean = false,
      closed: Boolean = false,
      dispatching: Boolean = false
  )

  def start(provider: SketchProvider, config: RunConfig)(
      create: SketchContext => Sketch
  ): SketchSession =
    provider.claim()
    val session = closeOnFailure(() => provider.close()) {
      provider.initialize(config.frameRate)
      require(
        provider.canvas.width > 0 && provider.canvas.height > 0,
        "provider must supply positive dimensions before sketch construction"
      )
      val sketch = create(SketchContext(provider.canvas, config.seed))
      new SketchSession(provider, sketch, FixedClock(config.clock))
    }
    session.initialize()
    session

  private def closeOnFailure[A](close: () => Unit)(body: => A): A =
    try body
    catch
      case error: Throwable =>
        Using.resource(close)(_ => throw error)(using releaseAction)

  private val releaseAction: Using.Releasable[() => Unit] = release => release()
