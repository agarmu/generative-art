package genart

import genart.lib.math.Vec
import genart.lib.random.{Rng, Seed}
import genart.lib.runtime.{RunConfig, SketchSession}
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

class SessionSuite extends munit.FunSuite:
  private val seed = Seed.fromLong(42L)

  test("initialization supplies context before setup") {
    val fixture = new RecordingSession
    import fixture.*
    assertEquals(
      provider.events.toVector,
      Vector("initialize", "construct", "setup")
    )
    session.stop()
  }

  test("an automatically generated seed can reproduce a session's randomness") {
    def samples(config: RunConfig): Vector[Long] =
      val values = ArrayBuffer.empty[Long]
      val session = SketchSession.start(new TestProvider, config) { context =>
        assertEquals(context.seed, config.seed)
        new Sketch:
          private var rng = Rng(context.seed)
          override def update(step: Step): Unit =
            val (value, next) = rng.nextLong
            values += value
            rng = next
          def draw(frame: Frame): Unit = ()
      }
      try
        session.replay(10)
        values.toVector
      finally session.stop()

    val automatic = RunConfig()
    val original = samples(automatic)
    assertEquals(original.size, 10)
    assertEquals(
      samples(RunConfig(Seed.fromHex(automatic.seed.toHex))),
      original
    )
  }

  test("a provider cannot be claimed by a second session") {
    val fixture = new RecordingSession
    import fixture.*
    intercept[IllegalArgumentException] {
      SketchSession.start(provider, RunConfig(seed))(_ =>
        new Sketch:
          def draw(frame: Frame): Unit = ()
      )
    }
    assert(!session.isStopped && !provider.events.contains("close"))
    session.stop()
  }

  test(
    "frames dispatch updates before rendering, including frames without updates"
  ) {
    val fixture = new RecordingSession
    import fixture.*
    session.frame(0.0)
    assert(steps.isEmpty && frames.size == 1)
    assertEquals(
      provider.events.takeRight(2).toVector,
      Vector("beginFrame", "draw")
    )
    session.frame(3.0 / 60.0)
    assertEquals(steps.map(_.index).toVector, Vector(1L, 2L, 3L))
    assertEquals(
      provider.events.takeRight(5).toVector,
      Vector("update", "update", "update", "beginFrame", "draw")
    )
    session.frame(3.0 / 60.0)
    assert(steps.size == 3 && frames.size == 3)
    session.stop()
  }

  test(
    "suspension skips frames and resumption resets the wall-clock baseline"
  ) {
    val fixture = new RecordingSession
    import fixture.*
    session.frame(0.0)
    session.frame(3.0 / 60.0)
    val stepsBefore = steps.size
    val framesBefore = frames.size
    session.suspend()
    session.frame(100.0)
    assert(session.isSuspended && frames.size == framesBefore)
    session.resume()
    session.frame(1000.0)
    assert(steps.size == stepsBefore && frames.size == framesBefore + 1)
    session.frame(1000.051)
    assertEquals(steps.size, stepsBefore + 3)
    session.stop()
  }

  test(
    "resize updates dimensions before notification and stop disposes exactly once"
  ) {
    val fixture = new RecordingSession
    import fixture.*
    session.resize(800, 600)
    assertEquals(
      provider.events.takeRight(2).toVector,
      Vector("resize", "resized")
    )
    session.stop()
    session.stop()
    session.frame(2000.0)
    session.replay(10)
    assert(session.isStopped)
    assertEquals(provider.events.count(_ == "dispose"), 1)
    assertEquals(provider.events.count(_ == "close"), 1)
    assert(steps.isEmpty && frames.isEmpty)
  }

  test("explicit replay advances the requested number of ticks") {
    val replayProvider = new TestProvider
    val replaySteps = ArrayBuffer.empty[Step]
    val replaySession = SketchSession.start(replayProvider, RunConfig(seed)) {
      _ =>
        new Sketch:
          override def update(step: Step): Unit = replaySteps += step
          def draw(frame: Frame): Unit = ()
    }
    replaySession.replay(60)
    assert(replaySteps.size == 60)
    assert(replaySteps.last.seconds == 1.0)
    assert(!replaySession.isStopped)
    replaySession.stop()
  }

  test(
    "input events precede updates and are ignored while suspended or stopped"
  ) {
    val fixture = new RecordingSession
    import fixture.*
    val press = PointerEvent.Pressed(Vec(20.0, 30.0), PointerButton.Primary)
    val release = PointerEvent.Released(Vec(40.0, 50.0), PointerButton.Primary)
    session.pointer(press)
    val key = KeyEvent.Pressed(Key.Character("r"))
    session.key(key)
    session.replay(1)
    assertEquals(
      provider.events.takeRight(5).toVector,
      Vector("pointer", "key", "update", "beginFrame", "draw")
    )
    session.suspend()
    session.pointer(PointerEvent.Moved(Vec(0.0, 0.0)))
    session.key(KeyEvent.Cancelled)
    session.resume()
    session.pointer(release)
    session.key(KeyEvent.Released(Key.Character("r")))
    session.stop()
    session.pointer(press)
    session.key(key)
    assertEquals(pointers.toVector, Vector(press, release))
    assertEquals(
      keys.toVector,
      Vector(key, KeyEvent.Released(Key.Character("r")))
    )
  }

  test("setup failures dispose the sketch and close the provider") {
    val setupProvider = new TestProvider
    val setupFailure = new RuntimeException("setup")
    assert(Try(SketchSession.start(setupProvider, RunConfig(seed)) { _ =>
      new Sketch:
        override def setup(): Unit = throw setupFailure
        def draw(frame: Frame): Unit = ()
        override def dispose(): Unit = setupProvider.events += "dispose"
    }).failed.get eq setupFailure)
    assert(
      setupProvider.events.takeRight(2).toVector == Vector("dispose", "close")
    )
  }

  test(
    "update failure preserves the error and closes despite disposal failure"
  ) {
    val failedProvider = new TestProvider
    val failure = new RuntimeException("update")
    val cleanupFailure = new RuntimeException("dispose")
    val failed = SketchSession.start(failedProvider, RunConfig(seed)) { _ =>
      new Sketch:
        override def update(step: Step): Unit = throw failure
        def draw(frame: Frame): Unit = ()
        override def dispose(): Unit = throw cleanupFailure
    }
    assert(Try(failed.replay(1)).failed.get eq failure)
    assert(failed.isStopped && failedProvider.events.count(_ == "close") == 1)
    assert(failure.getSuppressed.toVector == Vector(cleanupFailure))
  }

  test("factory failures close the provider without constructing a session") {
    val failure = new RuntimeException("factory")
    val factoryProvider = new TestProvider
    assert(
      Try(
        SketchSession.start(factoryProvider, RunConfig(seed))(_ =>
          throw failure
        )
      ).failed.get eq failure
    )
    assert(factoryProvider.events.toVector == Vector("initialize", "close"))
  }

  test("stopping during update skips later updates and drawing") {
    val stoppingProvider = new TestProvider
    var handle = Option.empty[SketchSession]
    val stoppedSteps = ArrayBuffer.empty[Step]
    val stopping = SketchSession.start(stoppingProvider, RunConfig(seed)) { _ =>
      new Sketch:
        override def update(step: Step): Unit =
          stoppedSteps += step
          handle.foreach(_.stop())
        def draw(frame: Frame): Unit = stoppingProvider.events += "draw"
    }
    handle = Some(stopping)
    stopping.replay(5)
    assert(stoppedSteps.size == 1)
    assert(!stoppingProvider.events.contains("draw"))
  }

  test("stopping inside a scope restores drawing state before teardown") {
    val scopedProvider = new TestProvider
    var scopedHandle = Option.empty[SketchSession]
    val scoped = SketchSession.start(scopedProvider, RunConfig(seed)) {
      context =>
        new Sketch:
          def draw(frame: Frame): Unit =
            context.canvas.scoped {
              scopedHandle.foreach(_.stop())
              assert(!scopedProvider.events.contains("close"))
            }
            scopedProvider.events += "drawEnd"
          override def dispose(): Unit = scopedProvider.events += "dispose"
    }
    scopedHandle = Some(scoped)
    scoped.replay(0)
    assert(
      scopedProvider.events.takeRight(4).toVector == Vector(
        "pop",
        "drawEnd",
        "dispose",
        "close"
      )
    )
  }

  test("fatal disposal failures still close the provider") {
    val fatalProvider = new TestProvider
    val fatal = new LinkageError("dispose")
    val fatalSession = SketchSession.start(fatalProvider, RunConfig(seed)) {
      _ =>
        new Sketch:
          def draw(frame: Frame): Unit = ()
          override def dispose(): Unit = throw fatal
    }
    try
      fatalSession.stop()
      assert(false, "expected disposal failure")
    catch case error: LinkageError => assert(error eq fatal)
    assert(
      fatalSession.isStopped && fatalProvider.events.count(_ == "close") == 1
    )
  }

  test("overflowing wall-clock differences are clamped") {
    val wideTimeProvider = new TestProvider
    val wideSteps = ArrayBuffer.empty[Step]
    val wideTime = SketchSession.start(wideTimeProvider, RunConfig(seed)) { _ =>
      new Sketch:
        override def update(step: Step): Unit = wideSteps += step
        def draw(frame: Frame): Unit = ()
    }
    wideTime.frame(-Double.MaxValue)
    wideTime.frame(Double.MaxValue)
    assert(wideSteps.size == 5 && !wideTime.isStopped)
    wideTime.stop()
  }

  private class RecordingSession:
    val provider = new TestProvider
    val steps = ArrayBuffer.empty[Step]
    val frames = ArrayBuffer.empty[Frame]
    val pointers = ArrayBuffer.empty[PointerEvent]
    val keys = ArrayBuffer.empty[KeyEvent]
    val session = SketchSession.start(provider, RunConfig(seed)) { context =>
      assert(context.seed == seed)
      assert(context.canvas.width == 640 && context.canvas.height == 480)
      provider.events += "construct"
      new Sketch:
        override def setup(): Unit = provider.events += "setup"
        override def pointer(event: PointerEvent): Unit =
          pointers += event
          provider.events += "pointer"
        override def key(event: KeyEvent): Unit =
          keys += event
          provider.events += "key"
        override def update(step: Step): Unit =
          steps += step
          provider.events += "update"
        def draw(frame: Frame): Unit =
          frames += frame
          provider.events += "draw"
        override def resized(): Unit =
          assert(context.canvas.width == 800 && context.canvas.height == 600)
          provider.events += "resized"
        override def dispose(): Unit = provider.events += "dispose"
    }

private[genart] class TestProvider extends SketchProvider:
  val events = ArrayBuffer.empty[String]
  private var dimensions = (0, 0)
  val canvas: Canvas = new Canvas:
    def width: Int = dimensions._1
    def height: Int = dimensions._2
    def snapshot(): Image =
      throw new UnsupportedOperationException("test canvas has no renderer")
    protected def clearTransparent(): Unit = ()
    protected def createSurface(width: Int, height: Int): Surface =
      throw new UnsupportedOperationException("test canvas has no renderer")
    protected def setStrokeColor(color: Color): Unit = ()
    protected def setFillColor(color: Color): Unit = ()
    protected def disableStroke(): Unit = ()
    protected def disableFill(): Unit = ()
    protected def setStrokeWeight(weight: Double): Unit = ()
    protected def setStrokeCap(cap: StrokeCap): Unit = ()
    protected def setStrokeJoin(join: StrokeJoin): Unit = ()
    protected def drawPath(path: Path, rule: FillRule): Unit = ()
    protected def intersectClip(path: Path, rule: FillRule): Unit = ()
    protected def measureTextLine(
        value: String,
        style: TextStyle
    ): TextMetrics =
      TextMetrics(0.0, 0.0, 0.0, 0.0)
    protected def drawTextLine(
        value: String,
        position: Vec[2],
        style: TextStyle
    ): Unit = ()
    protected def drawImage(
        source: Image,
        position: Vec[2],
        size: Vec[2],
        sourcePosition: Vec[2],
        sourceSize: Vec[2]
    ): Unit = ()
    protected def clearSurface(color: Color): Unit = ()
    protected def drawRect(position: Vec[2], size: Vec[2]): Unit = ()
    protected def drawEllipse(center: Vec[2], size: Vec[2]): Unit = ()
    protected def drawLine(from: Vec[2], to: Vec[2]): Unit = ()
    protected def drawTriangle(a: Vec[2], b: Vec[2], c: Vec[2]): Unit = ()
    protected def drawQuad(a: Vec[2], b: Vec[2], c: Vec[2], d: Vec[2]): Unit =
      ()
    protected def translateBy(offset: Vec[2]): Unit = ()
    protected def rotateBy(radians: Double): Unit = ()
    protected def scaleBy(factors: Vec[2]): Unit = ()
    protected def pushState(): Unit = events += "push"
    protected def popState(): Unit = events += "pop"

  def initialize(frameRate: Double): Unit =
    dimensions = (640, 480)
    events += "initialize"
  def beginFrame(): Unit = events += "beginFrame"
  def resize(width: Int, height: Int): Unit =
    dimensions = (width, height)
    events += "resize"
  def close(): Unit = events += "close"
