package genart

import genart.lib.math.Vec
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

class CanvasSuite extends munit.FunSuite:
  test(
    "nested scopes restore state after success and failure without undoing drawing"
  ) {
    val canvas = new RecordingCanvas
    val red = Color.rgb(1.0, 0.0, 0.0)
    val blue = Color.rgb(0.0, 0.0, 1.0)
    canvas.style(Style(Some(red), None, 2.0))
    canvas.translate(Vec(4.0, 5.0))
    val initial = canvas.state
    val failure = new RuntimeException("drawing")
    val result = canvas.scoped {
      canvas.style(Style(None, Some(blue), 3.0))
      canvas.rotate(0.5)
      val outer = canvas.state
      val thrown = Try(canvas.scoped {
        canvas.style(Style(Some(blue), Some(red), 4.0))
        canvas.scale(2.0, 3.0)
        canvas.line(Vec(1.0, 2.0), Vec(3.0, 4.0))
        throw failure
      }).failed.get
      assert(thrown eq failure)
      assert(canvas.state == outer)
      42
    }
    assert(result == 42 && canvas.state == initial)
    assert(canvas.lines.size == 1)
    assert(canvas.lines.head._1 == Vector(1.0, 2.0, 3.0, 4.0))
    assert(canvas.lines.head._2.fill == Some(red))

  }

  test("scope restoration failures are suppressed by the drawing failure") {
    val canvas = new RecordingCanvas
    val initial = canvas.state
    val failure = new RuntimeException("drawing")
    val restoreFailure = new RuntimeException("restore")
    canvas.restoreFailure = Some(restoreFailure)
    assert(Try(canvas.scoped(throw failure)).failed.get eq failure)
    assert(failure.getSuppressed.toVector == Vector(restoreFailure))
    canvas.restoreFailure = None
    assert(canvas.state == initial)

  }

  test("invalid drawing inputs are rejected before changing canvas state") {
    val canvas = new RecordingCanvas
    val initial = canvas.state
    val red = Color.rgb(1.0, 0.0, 0.0)
    for invalid <- List(-1.0, Double.NaN, Double.PositiveInfinity) do
      assertRejected(canvas.strokeWeight(invalid))
    assertRejected(canvas.background(red.withAlpha(0.5)))
    assertRejected(canvas.rect(0.0, 0.0, -1.0, 5.0))
    assertRejected(canvas.ellipse(0.0, 0.0, -1.0, 5.0))
    assertRejected(canvas.ellipse(Vec(0.0, Double.NaN), Vec(1.0, 2.0)))
    assertRejected(canvas.ellipse(0.0, 0.0, 1.0, Double.PositiveInfinity))
    assertRejected(canvas.circle(0.0, 0.0, -1.0))
    assertRejected(canvas.circle(Vec(0.0, 0.0), Double.NaN))
    assertRejected(canvas.line(0.0, Double.NaN, 1.0, 1.0))
    assertRejected(canvas.rotate(Double.PositiveInfinity))
    assert(canvas.state == initial && canvas.lines.isEmpty)
  }

  private def assertRejected(body: => Any): Unit =
    intercept[IllegalArgumentException](body)

  private case class State(
      stroke: Option[Color] = None,
      fill: Option[Color] = None,
      weight: Double = 1.0,
      transforms: Vector[(String, Double, Double)] = Vector.empty
  )

  private class RecordingCanvas extends Canvas:
    val width = 640
    val height = 480
    def snapshot(): Image = fail("recording canvas has no renderer")
    var state = State()
    var restoreFailure = Option.empty[Throwable]
    val lines = ArrayBuffer.empty[(Vector[Double], State)]
    private var saved = List.empty[State]
    protected def setStrokeColor(color: Color): Unit = state =
      state.copy(stroke = Some(color))
    protected def setFillColor(color: Color): Unit = state =
      state.copy(fill = Some(color))
    protected def disableStroke(): Unit = state = state.copy(stroke = None)
    protected def disableFill(): Unit = state = state.copy(fill = None)
    protected def setStrokeWeight(weight: Double): Unit = state =
      state.copy(weight = weight)
    protected def clearSurface(color: Color): Unit = ()
    protected def drawRect(
        x: Double,
        y: Double,
        width: Double,
        height: Double
    ): Unit = ()
    protected def drawEllipse(
        centerX: Double,
        centerY: Double,
        width: Double,
        height: Double
    ): Unit = fail("invalid ellipse reached the renderer")
    protected def drawLine(
        x1: Double,
        y1: Double,
        x2: Double,
        y2: Double
    ): Unit =
      lines += ((Vector(x1, y1, x2, y2), state))
    protected def translateBy(x: Double, y: Double): Unit =
      transform("translate", x, y)
    protected def rotateBy(radians: Double): Unit =
      transform("rotate", radians, 0.0)
    protected def scaleBy(x: Double, y: Double): Unit = transform("scale", x, y)
    protected def pushState(): Unit = saved = state :: saved
    protected def popState(): Unit =
      state = saved.head
      saved = saved.tail
      restoreFailure.foreach(throw _)
    private def transform(name: String, x: Double, y: Double): Unit =
      state = state.copy(transforms = state.transforms :+ ((name, x, y)))
