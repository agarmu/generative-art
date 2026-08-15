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
    canvas.style(Style(Some(red), None, 2.0, StrokeCap.Butt, StrokeJoin.Bevel))
    canvas.translate(Vec(4.0, 5.0))
    val initial = canvas.state
    val failure = new RuntimeException("drawing")
    val result = canvas.scoped {
      canvas.style(
        Style(None, Some(blue), 3.0, StrokeCap.Square, StrokeJoin.Round)
      )
      canvas.rotate(0.5)
      val outer = canvas.state
      val thrown = Try(canvas.scoped {
        canvas.style(Style(Some(blue), Some(red), 4.0))
        canvas.scale(Vec(2.0, 3.0))
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
    assertRejected(canvas.rect(Vec(0.0, 0.0), Vec(-1.0, 5.0)))
    assertRejected(canvas.ellipse(Vec(0.0, 0.0), Vec(-1.0, 5.0)))
    assertRejected(canvas.ellipse(Vec(0.0, Double.NaN), Vec(1.0, 2.0)))
    assertRejected(
      canvas.ellipse(Vec(0.0, 0.0), Vec(1.0, Double.PositiveInfinity))
    )
    assertRejected(canvas.circle(Vec(0.0, 0.0), -1.0))
    assertRejected(canvas.circle(Vec(0.0, 0.0), Double.NaN))
    assertRejected(TextStyle(size = 0.0))
    assertRejected(TextStyle(lineHeight = Double.NaN))
    assertRejected(canvas.text("hello", Vec(Double.NaN, 0.0)))
    val image = new Image(2, 2):
      protected def read(x: Int, y: Int): Color = red
    assertRejected(canvas.image(image, Vec(0.0, 0.0), Vec(-1.0, 2.0)))
    assertRejected(
      canvas.image(
        image,
        Vec(0.0, 0.0),
        Vec(2.0, 2.0),
        Vec(1.0, 0.0),
        Vec(2.0, 2.0)
      )
    )
    canvas.image(image, Vec(0.0, 0.0), Vec(0.0, 2.0))
    assertRejected(
      canvas.triangle(Vec(0.0, 0.0), Vec(1.0, 0.0), Vec(Double.NaN, 1.0))
    )
    assertRejected(
      canvas.triangle(
        Vec(0.0, 0.0),
        Vec(1.0, 0.0),
        Vec(0.0, Double.PositiveInfinity)
      )
    )
    assertRejected(
      canvas.quad(
        Vec(0.0, 0.0),
        Vec(1.0, 0.0),
        Vec(1.0, 1.0),
        Vec(0.0, Double.NaN)
      )
    )
    assertRejected(
      canvas.quad(
        Vec(0.0, 0.0),
        Vec(1.0, 0.0),
        Vec(1.0, 1.0),
        Vec(Double.NegativeInfinity, 1.0)
      )
    )
    assertRejected(canvas.line(Vec(0.0, Double.NaN), Vec(1.0, 1.0)))
    assertRejected(canvas.rotate(Double.PositiveInfinity))
    assertRejected(canvas.translate(Vec(0.0, Double.NaN)))
    assertRejected(canvas.scale(Vec(Double.PositiveInfinity, 1.0)))
    assertRejected(canvas.arc(Vec(0.0, 0.0), Vec(20.0, 10.0), 0.0, -7.0))
    assertRejected(
      canvas.path(Path(Vec(0.0, 0.0)).lineTo(Vec(Double.NaN, 0.0)))
    )
    assertRejected(
      canvas.clipped(Path(Vec(0.0, 0.0)).moveTo(Vec(Double.NaN, 0.0))) {
        fail("invalid clip entered its body")
      }
    )
    assert(canvas.state == initial && canvas.lines.isEmpty)
  }

  test("multiline text measurement and alignment share the same layout") {
    val canvas = new RecordingCanvas
    val style = TextStyle(
      size = 10.0,
      align = TextAlign.Right,
      baseline = TextBaseline.Bottom,
      lineHeight = 2.0
    )
    assertEquals(
      canvas.measureText("wide\r\nx", style),
      TextMetrics(20.0, 30.0, 8.0, 2.0)
    )
    canvas.text("wide\nx", Vec(100.0, 100.0), style)
    assertEquals(
      canvas.texts.toVector,
      Vector("wide" -> Vec(80.0, 78.0), "x" -> Vec(95.0, 98.0))
    )
  }

  private def assertRejected(body: => Any): Unit =
    intercept[IllegalArgumentException](body)

  private case class State(
      stroke: Option[Color] = None,
      fill: Option[Color] = None,
      weight: Double = 1.0,
      cap: StrokeCap = StrokeCap.Round,
      join: StrokeJoin = StrokeJoin.Miter,
      transforms: Vector[(String, Double, Double)] = Vector.empty
  )

  private class RecordingCanvas extends Canvas:
    val width = 640
    val height = 480
    def snapshot(): Image = fail("recording canvas has no renderer")
    var state = State()
    var restoreFailure = Option.empty[Throwable]
    val lines = ArrayBuffer.empty[(Vector[Double], State)]
    val texts = ArrayBuffer.empty[(String, Vec[2])]
    private var saved = List.empty[State]
    protected def clearTransparent(): Unit = fail("unexpected clear")
    protected def createSurface(width: Int, height: Int): Surface =
      fail("recording canvas has no renderer")
    protected def setBlendMode(value: BlendMode): Unit = fail(
      "unexpected blend"
    )
    protected def setTint(color: Option[Color]): Unit = fail("unexpected tint")
    protected def setFillGradient(value: Gradient): Unit = fail(
      "unexpected gradient"
    )
    protected def setStrokeGradient(value: Gradient): Unit = fail(
      "unexpected gradient"
    )
    protected def setStrokeColor(color: Color): Unit = state =
      state.copy(stroke = Some(color))
    protected def setFillColor(color: Color): Unit = state =
      state.copy(fill = Some(color))
    protected def disableStroke(): Unit = state = state.copy(stroke = None)
    protected def disableFill(): Unit = state = state.copy(fill = None)
    protected def setStrokeWeight(weight: Double): Unit = state =
      state.copy(weight = weight)
    protected def setStrokeCap(cap: StrokeCap): Unit = state =
      state.copy(cap = cap)
    protected def setStrokeJoin(join: StrokeJoin): Unit = state =
      state.copy(join = join)
    protected def drawPath(path: Path, rule: FillRule): Unit = fail(
      "invalid path reached the renderer"
    )
    protected def intersectClip(path: Path, rule: FillRule): Unit = ()
    protected def drawImage(
        source: Image,
        position: Vec[2],
        size: Vec[2],
        sourcePosition: Vec[2],
        sourceSize: Vec[2]
    ): Unit = fail("invalid image draw reached the renderer")
    protected def clearSurface(color: Color): Unit = ()
    protected def drawRect(position: Vec[2], size: Vec[2]): Unit = ()
    protected def drawEllipse(center: Vec[2], size: Vec[2]): Unit = fail(
      "invalid ellipse reached the renderer"
    )
    protected def drawLine(from: Vec[2], to: Vec[2]): Unit =
      lines += ((Vector(from.x, from.y, to.x, to.y), state))
    protected def measureTextLine(
        value: String,
        style: TextStyle
    ): TextMetrics =
      TextMetrics(
        value.length * style.size * 0.5,
        style.size,
        style.size * 0.8,
        style.size * 0.2
      )
    protected def drawTextLine(
        value: String,
        position: Vec[2],
        style: TextStyle
    ): Unit =
      texts += value -> position
    protected def drawTriangle(a: Vec[2], b: Vec[2], c: Vec[2]): Unit = fail(
      "invalid triangle reached the renderer"
    )
    protected def drawQuad(a: Vec[2], b: Vec[2], c: Vec[2], d: Vec[2]): Unit =
      fail("invalid quad reached the renderer")
    protected def translateBy(offset: Vec[2]): Unit =
      transform("translate", offset.x, offset.y)
    protected def rotateBy(radians: Double): Unit =
      transform("rotate", radians, 0.0)
    protected def scaleBy(factors: Vec[2]): Unit =
      transform("scale", factors.x, factors.y)
    protected def pushState(): Unit = saved = state :: saved
    protected def popState(): Unit =
      state = saved.head
      saved = saved.tail
      restoreFailure.foreach(throw _)
    private def transform(name: String, x: Double, y: Double): Unit =
      state = state.copy(transforms = state.transforms :+ ((name, x, y)))
