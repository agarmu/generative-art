package genart.processing

import genart.{
  ArcMode,
  Color,
  FillRule,
  Frame,
  Image,
  Key,
  KeyEvent,
  KeyModifiers,
  Path,
  Sketch,
  Step,
  StrokeCap,
  StrokeJoin,
  FontFamily,
  TextBaseline,
  TextStyle
}
import genart.lib.math.Vec
import genart.lib.random.Seed
import genart.lib.runtime.{RunConfig, SketchSession}
import processing.awt.PGraphicsJava2D
import processing.core.{PApplet, PConstants, PSurfaceNone}

class ProcessingSuite extends munit.FunSuite:
  private val black = Color.rgb(0.0, 0.0, 0.0)
  private val red = Color.rgb(1.0, 0.0, 0.0)
  private val green = Color.rgb(0.0, 1.0, 0.0)
  private val blue = Color.rgb(0.0, 0.0, 1.0)

  private def renderAt(density: Int) =
    FunFixture[(PApplet, ProcessingProvider)](
      setup = _ => {
        val applet = new PApplet:
          pixelDensity = density
          setSize(64, 64)
          g = new PGraphicsJava2D
          g.setParent(this)
          g.setPrimary(false)
          g.setSize(width, height)
          surface = new PSurfaceNone(g)
          surface.initOffscreen(this)
          override def dispose(): Unit =
            surface.stopThread()
            g.dispose()
        applet.g.beginDraw()
        (applet, new ProcessingProvider(applet))
      },
      teardown = { case (applet, provider) =>
        try applet.g.endDraw()
        finally provider.close()
      }
    )

  private val rendering = renderAt(1)
  private val denseRendering = renderAt(2)

  test(
    "native image adapters copy decoded pixels and preserve physical dimensions"
  ) {
    val original = new java.awt.image.BufferedImage(
      2,
      1,
      java.awt.image.BufferedImage.TYPE_INT_ARGB
    )
    original.setRGB(0, 0, 0x80ff0000)
    val encoded = new java.io.ByteArrayOutputStream
    javax.imageio.ImageIO.write(original, "png", encoded)
    val decoded = javax.imageio.ImageIO.read(
      new java.io.ByteArrayInputStream(encoded.toByteArray)
    )
    val image = ProcessingImage.from(decoded)
    decoded.setRGB(0, 0, 0xff000000)
    assertEquals((image.width, image.height), (2, 1))
    assertEquals(image(0, 0), red.withAlpha(128.0 / 255.0))
    encoded.reset()
    ProcessingImage.writePng(image, encoded)
    val exported = javax.imageio.ImageIO.read(
      new java.io.ByteArrayInputStream(encoded.toByteArray)
    )
    assertEquals(exported.getRGB(0, 0), 0x80ff0000)

    val native = new processing.core.PImage(2, 1, PConstants.ARGB, 2)
    native.pixels(5) = 0xff0000ff
    val captured = ProcessingImage.from(native)
    native.pixels(5) = 0
    assertEquals((captured.width, captured.height), (4, 2))
    assertEquals(captured(1, 1), blue)
    intercept[IllegalArgumentException](
      ProcessingImage.from(new processing.core.PImage())
    )
  }

  test(
    "keyboard repeats and releases retain the pressed key until cancellation"
  ) {
    val events = scala.collection.mutable.ArrayBuffer.empty[KeyEvent]
    val input = new ProcessingKeyboardInput(events += _)
    def event(action: Int, character: Char, modifiers: Int = 0) =
      new processing.event.KeyEvent(
        null,
        0L,
        action,
        modifiers,
        character,
        java.awt.event.KeyEvent.VK_A
      )
    input.down(
      event(processing.event.KeyEvent.PRESS, 'A', processing.event.Event.SHIFT)
    )
    input.down(event(processing.event.KeyEvent.PRESS, 'a'))
    input.up(event(processing.event.KeyEvent.RELEASE, 'a'))
    input.down(event(processing.event.KeyEvent.PRESS, 'a'))
    input.cancel()
    input.up(event(processing.event.KeyEvent.RELEASE, 'a'))
    assertEquals(
      events.toVector,
      Vector(
        KeyEvent.Pressed(Key.Character("a"), KeyModifiers(shift = true)),
        KeyEvent.Pressed(Key.Character("a"), repeat = true),
        KeyEvent.Released(Key.Character("a")),
        KeyEvent.Pressed(Key.Character("a")),
        KeyEvent.Cancelled
      )
    )
  }

  rendering.test("text uses native metrics and preserves drawing state") {
    case (applet, provider) =>
      provider.initialize(60.0)
      val canvas = provider.canvas
      canvas.background(black)
      canvas.noStroke()
      canvas.fill(green)
      canvas.translate(Vec(8.0, 8.0))
      val style = TextStyle(
        size = 20.0,
        family = FontFamily.Monospace,
        baseline = TextBaseline.Top
      )
      val measured = canvas.measureText("M", style)
      assert(measured.width > 0.0 && measured.height > 0.0)
      assertEqualsDouble(
        canvas.measureText("MMMM", style).width,
        measured.width * 4.0,
        0.01
      )
      canvas.text("M", Vec(0.0, 0.0), style)
      val image = canvas.snapshot()
      assert((for y <- 8 until 32; x <- 8 until 32
      yield image(x, y).green).exists(_ > 0.5))
      assertEquals(applet.g.fillColor, 0xff00ff00)
      canvas.rect(Vec(24.0, 24.0), Vec(8.0, 8.0))
      assertEquals(canvas.snapshot()(36, 36), green)
  }

  rendering.test("initialization establishes portable drawing defaults") {
    case (applet, provider) =>
      applet.g.colorMode(PConstants.HSB, 360.0f, 100.0f, 100.0f, 100.0f)
      applet.g.noFill()
      applet.g.noStroke()
      applet.g.rectMode(PConstants.CENTER)
      applet.g.strokeCap(PConstants.PROJECT)
      applet.g.strokeJoin(PConstants.BEVEL)
      provider.initialize(60.0)
      assert(applet.g.fill && applet.g.fillColor == 0xffffffff)
      assert(applet.g.stroke && applet.g.strokeColor == 0xff000000)
      assertEquals(applet.g.strokeWeight, 1.0f)
      assertEquals(applet.g.strokeCap, PConstants.ROUND)
      assertEquals(applet.g.strokeJoin, PConstants.MITER)
      provider.canvas.noStroke()
      provider.canvas.fill(red)
      provider.canvas.rect(Vec(0.0, 0.0), Vec(8.0, 8.0))
      assertEquals(provider.canvas.snapshot()(6, 6), red)
  }

  rendering.test(
    "ellipses use center coordinates and circles use diameter"
  ) { case (applet, provider) =>
    applet.g.ellipseMode(PConstants.CORNER)
    provider.initialize(60.0)
    val canvas = provider.canvas
    canvas.background(black)
    canvas.noStroke()
    canvas.fill(red)
    canvas.ellipse(Vec(16.0, 16.0), Vec(24.0, 12.0))
    canvas.fill(green)
    canvas.circle(Vec(46.0, 16.0), 16.0)
    val image = canvas.snapshot()
    assertEquals(image(16, 16), red)
    assertEquals(image(8, 16), red)
    assertEquals(image(16, 12), red)
    assertEquals(image(16, 8), black)
    assertEquals(image(25, 21), black)
    assertEquals(image(46, 16), green)
    assertEquals(image(52, 16), green)
    assertEquals(image(46, 22), green)
    assertEquals(image(55, 16), black)
    assertEquals(image(46, 25), black)
  }

  rendering.test("triangles and quads use the supplied vertices") {
    case (_, provider) =>
      provider.initialize(60.0)
      val canvas = provider.canvas
      canvas.background(black)
      canvas.noStroke()
      canvas.fill(red)
      canvas.triangle(Vec(4.0, 4.0), Vec(28.0, 4.0), Vec(4.0, 28.0))
      canvas.fill(green)
      canvas.quad(
        Vec(36.0, 4.0),
        Vec(60.0, 8.0),
        Vec(56.0, 28.0),
        Vec(32.0, 24.0)
      )
      val image = canvas.snapshot()
      assertEquals(image(8, 8), red)
      assertEquals(image(20, 8), red)
      assertEquals(image(24, 24), black)
      assertEquals(image(46, 16), green)
      assertEquals(image(36, 20), green)
      assertEquals(image(60, 24), black)
      assertEquals(image(32, 4), black)
  }

  rendering.test("arcs and mixed paths render through the native provider") {
    case (_, provider) =>
      provider.initialize(60.0)
      val canvas = provider.canvas
      canvas.background(black)
      canvas.noStroke()
      canvas.fill(red)
      canvas.path(
        Path(Vec(8.0, 48.0))
          .lineTo(Vec(8.0, 32.0))
          .quadraticTo(Vec(32.0, 0.0), Vec(56.0, 32.0))
          .arcTo(Vec(56.0, 40.0), Vec(16.0, 16.0), -math.Pi / 2.0, math.Pi)
          .bezierTo(Vec(56.0, 56.0), Vec(8.0, 56.0), Vec(8.0, 48.0))
          .close
      )
      canvas.fill(green)
      canvas.arc(
        Vec(16.0, 16.0),
        Vec(24.0, 24.0),
        0.0,
        math.Pi / 2.0,
        ArcMode.Pie
      )
      val image = canvas.snapshot()
      assertEquals(image(32, 32), red)
      assertEquals(image(32, 10), black)
      assertEquals(image(20, 20), green)
  }

  rendering.test(
    "compound contours support both fill rules and reversed arcs"
  ) { case (_, provider) =>
    provider.initialize(60.0)
    val canvas = provider.canvas
    val nested = Path(Vec(4.0, 4.0))
      .lineTo(Vec(28.0, 4.0))
      .lineTo(Vec(28.0, 28.0))
      .lineTo(Vec(4.0, 28.0))
      .close
      .moveTo(Vec(10.0, 10.0))
      .lineTo(Vec(22.0, 10.0))
      .lineTo(Vec(22.0, 22.0))
      .lineTo(Vec(10.0, 22.0))
      .close
    canvas.background(black)
    canvas.noStroke()
    canvas.fill(red)
    canvas.path(nested)
    canvas.scoped {
      canvas.translate(Vec(32.0, 0.0))
      canvas.path(nested, FillRule.EvenOdd)
    }
    val center = Vec(16.0, 48.0)
    canvas.path(
      Path.arc(center, Vec(24.0, 24.0), 0.0, 2.0 * math.Pi) ++
        Path.arc(center, Vec(12.0, 12.0), 0.0, -2.0 * math.Pi)
    )
    val result = canvas.snapshot()
    assertEquals(result(16, 16), red)
    assertEquals(result(48, 16), black)
    assertEquals(result(40, 16), red)
    assertEquals(result(16, 48), black)
    assertEquals(result(25, 48), red)
  }

  rendering.test("nested clipping intersects and restores after exceptions") {
    case (applet, provider) =>
      provider.initialize(60.0)
      val canvas = provider.canvas
      val center = Vec(32.0, 32.0)
      val ring = Path.arc(center, Vec(48.0, 48.0), 0.0, 2.0 * math.Pi) ++
        Path.arc(center, Vec(16.0, 16.0), 0.0, 2.0 * math.Pi)
      val quarter = Path(Vec(0.0, 0.0))
        .lineTo(Vec(32.0, 0.0))
        .lineTo(Vec(32.0, 32.0))
        .lineTo(Vec(0.0, 32.0))
        .close
      canvas.background(black)
      canvas.noStroke()
      canvas.fill(red)
      canvas.clipped(ring, FillRule.EvenOdd) {
        canvas.background(red)
        canvas.clipped(quarter) {
          canvas.fill(green)
          canvas.rect(Vec(0.0, 0.0), Vec(64.0, 64.0))
        }
        intercept[RuntimeException] {
          canvas.clipped(quarter) {
            canvas.translate(Vec(100.0, 100.0))
            canvas.fill(green)
            throw new RuntimeException("draw")
          }
        }
        canvas.fill(blue)
        canvas.rect(Vec(0.0, 32.0), Vec(64.0, 32.0))
      }
      assertEquals(applet.g.fillColor, 0xffff0000)
      canvas.rect(Vec(0.0, 0.0), Vec(4.0, 4.0))
      canvas.clipped(Path(Vec(0.0, 0.0))) { canvas.background(green) }
      val result = canvas.snapshot()
      assertEquals(result(18, 18), green)
      assertEquals(result(18, 46), blue)
      assertEquals(result(32, 32), black)
      assertEquals(result(2, 2), red)
  }

  rendering.test("elliptical arcs preserve angles and tiny sweeps stay tiny") {
    case (_, provider) =>
      provider.initialize(60.0)
      val canvas = provider.canvas
      canvas.background(black)
      canvas.noFill()
      canvas.stroke(green)
      canvas.strokeWeight(4.0)
      canvas.arc(Vec(32.0, 32.0), Vec(48.0, 16.0), math.Pi / 4.0, math.Pi / 2.0)
      assertEquals(canvas.snapshot()(17, 38), green)
      canvas.background(black)
      canvas.arc(Vec(32.0, 32.0), Vec(48.0, 48.0), 0.0, 1e-6)
      assertEquals(canvas.snapshot()(8, 32), black)
  }

  rendering.test(
    "nested scopes restore native style and transforms after failure"
  ) { case (applet, provider) =>
    provider.initialize(60.0)
    val canvas = provider.canvas
    canvas.background(black)
    canvas.strokeCap(StrokeCap.Butt)
    canvas.strokeJoin(StrokeJoin.Bevel)
    canvas.noStroke()
    canvas.fill(red)
    val result = canvas.scoped {
      canvas.translate(Vec(12.0, 0.0))
      canvas.fill(green)
      canvas.rect(Vec(0.0, 0.0), Vec(8.0, 8.0))
      canvas.scoped {
        canvas.translate(Vec(12.0, 0.0))
        canvas.fill(blue)
        canvas.rect(Vec(0.0, 0.0), Vec(8.0, 8.0))
      }
      canvas.rect(Vec(0.0, 12.0), Vec(8.0, 8.0))
      42
    }
    assertEquals(result, 42)
    val failure = new RuntimeException("draw")
    val thrown = intercept[RuntimeException] {
      canvas.scoped {
        canvas.translate(Vec(40.0, 40.0))
        canvas.noFill()
        canvas.stroke(blue)
        canvas.strokeCap(StrokeCap.Square)
        canvas.strokeJoin(StrokeJoin.Round)
        throw failure
      }
    }
    assert(thrown eq failure)
    assertEquals(applet.g.strokeCap, PConstants.SQUARE)
    assertEquals(applet.g.strokeJoin, PConstants.BEVEL)
    def nested(depth: Int): Unit = canvas.scoped {
      canvas.fill(blue)
      if depth > 0 then nested(depth - 1)
    }
    intercept[RuntimeException](nested(32))
    assertEquals(applet.g.fillColor, 0xffff0000)
    canvas.rect(Vec(0.0, 0.0), Vec(8.0, 8.0))
    canvas.line(Vec(48.0, 48.0), Vec(56.0, 48.0))
    val image = canvas.snapshot()
    assertEquals(image(4, 4), red)
    assertEquals(image(16, 4), green)
    assertEquals(image(28, 4), blue)
    assertEquals(image(16, 16), green)
    assertEquals(image(52, 48), black)
  }

  rendering.test(
    "frame reset clears transforms while preserving styles and pixels"
  ) { case (_, provider) =>
    provider.initialize(60.0)
    val canvas = provider.canvas
    canvas.background(black)
    canvas.noStroke()
    canvas.fill(red)
    canvas.rect(Vec(0.0, 0.0), Vec(8.0, 8.0))
    canvas.translate(Vec(40.0, 40.0))
    provider.beginFrame()
    canvas.rect(Vec(0.0, 24.0), Vec(8.0, 8.0))
    val image = canvas.snapshot()
    assertEquals(image(4, 4), red)
    assertEquals(image(4, 28), red)
  }

  rendering.test("native transforms compose around the translated origin") {
    case (_, provider) =>
      provider.initialize(60.0)
      val canvas = provider.canvas
      canvas.background(black)
      canvas.translate(Vec(36.0, 12.0))
      canvas.rotate(math.Pi / 2.0)
      canvas.scale(Vec(2.0, 1.0))
      canvas.stroke(green)
      canvas.strokeWeight(4.0)
      canvas.line(Vec(0.0, 0.0), Vec(8.0, 0.0))
      assertEquals(canvas.snapshot()(36, 20), green)
  }

  rendering.test("background ignores transforms and translucent fills blend") {
    case (_, provider) =>
      provider.initialize(60.0)
      val canvas = provider.canvas
      canvas.translate(Vec(10.0, 10.0))
      canvas.background(blue)
      val background = canvas.snapshot()
      assertEquals(background(0, 0), blue)
      assertEquals(background(63, 63), blue)
      provider.beginFrame()
      canvas.noStroke()
      canvas.fill(red.withAlpha(0.5))
      canvas.rect(Vec(0.0, 0.0), Vec(8.0, 8.0))
      val mixed = canvas.snapshot()(4, 4)
      assertEqualsDouble(mixed.red, 0.5, 1.0 / 255.0)
      assertEqualsDouble(mixed.blue, 0.5, 1.0 / 255.0)
  }

  rendering.test(
    "native float conversion rejects overflow and zero frame rates"
  ) { case (_, provider) =>
    provider.initialize(60.0)
    intercept[IllegalArgumentException] {
      provider.canvas.translate(Vec(Double.MaxValue, 0.0))
    }
    intercept[IllegalArgumentException] {
      provider.canvas.ellipse(Vec(0.0, 0.0), Vec(Double.MaxValue, 1.0))
    }
    intercept[IllegalArgumentException] {
      provider.canvas.triangle(
        Vec(0.0, 0.0),
        Vec(1.0, 0.0),
        Vec(Double.MaxValue, 1.0)
      )
    }
    intercept[IllegalArgumentException] {
      provider.canvas.quad(
        Vec(0.0, 0.0),
        Vec(1.0, 0.0),
        Vec(1.0, 1.0),
        Vec(0.0, Double.MaxValue)
      )
    }
    intercept[IllegalArgumentException] {
      provider.initialize(Double.MinPositiveValue)
    }
  }

  rendering.test(
    "resize updates dimensions, clears pixels and preserves style"
  ) { case (applet, provider) =>
    provider.initialize(60.0)
    val canvas = provider.canvas
    canvas.background(blue)
    canvas.noStroke()
    canvas.fill(red)
    canvas.strokeWeight(4.0)
    canvas.translate(Vec(30.0, 30.0))
    provider.resize(80, 48)
    assertEquals((canvas.width, canvas.height), (80, 48))
    assertEquals((applet.width, applet.height), (80, 48))
    assertEquals(canvas.snapshot()(79, 47), black)
    assert(!applet.g.stroke && applet.g.strokeWeight == 4.0f)
    canvas.rect(Vec(0.0, 0.0), Vec(8.0, 8.0))
    assertEquals(provider.canvas.snapshot()(4, 4), red)
  }

  rendering.test("observed native resizes reset contents and preserve style") {
    case (applet, provider) =>
      provider.initialize(60.0)
      val canvas = provider.canvas
      canvas.noStroke()
      canvas.fill(red)
      applet.g.endDraw()
      applet.getSurface.setSize(96, 72)
      applet.g.beginDraw()
      provider.resize(96, 72)
      assertEquals((canvas.width, canvas.height), (96, 72))
      assertEquals(canvas.snapshot()(4, 4), black)
      canvas.rect(Vec(0.0, 0.0), Vec(8.0, 8.0))
      assertEquals(provider.canvas.snapshot()(4, 4), red)
  }

  rendering.test("shared sessions drive the provider and dispose only once") {
    case (_, provider) =>
      var updates = 0
      var draws = 0
      var disposals = 0
      var resizedDimensions = Option.empty[(Int, Int)]
      val session =
        SketchSession.start(provider, RunConfig(Seed.fromLong(1L))) { context =>
          new Sketch:
            override def setup(): Unit =
              context.canvas.noStroke()
              context.canvas.fill(red)
              context.canvas.translate(Vec(20.0, 20.0))

            override def update(step: Step): Unit = updates += 1

            def draw(frame: Frame): Unit =
              draws += 1
              context.canvas.rect(Vec(0.0, 0.0), Vec(8.0, 8.0))

            override def resized(): Unit =
              resizedDimensions =
                Some(context.canvas.width -> context.canvas.height)

            override def dispose(): Unit = disposals += 1
        }
      session.replay(2)
      assertEquals((updates, draws), (2, 1))
      assertEquals(provider.canvas.snapshot()(4, 4), red)
      session.resize(80, 48)
      assertEquals(resizedDimensions, Some(80 -> 48))
      session.stop()
      session.stop()
      assertEquals(disposals, 1)
      assert(session.isStopped)
      intercept[IllegalArgumentException](provider.initialize(60.0))
  }

  rendering.test(
    "snapshots preserve drawing state and survive drawing, resize and close"
  ) { case (_, provider) =>
    provider.initialize(60.0)
    val canvas = provider.canvas
    canvas.background(black)
    canvas.noStroke()
    canvas.fill(red)
    canvas.translate(Vec(12.0, 16.0))
    canvas.rect(Vec(0.0, 0.0), Vec(8.0, 8.0))
    val captured = canvas.snapshot()
    assertEquals((captured.width, captured.height), (64, 64))
    assertEquals(captured(0, 0), black)
    assertEquals(captured(16, 20), red)
    canvas.rect(Vec(12.0, 0.0), Vec(8.0, 8.0))
    assertEquals(canvas.snapshot()(28, 20), red)
    assertEquals(captured(28, 20), black)
    provider.beginFrame()
    canvas.background(blue)
    provider.resize(80, 48)
    provider.close()
    assertEquals((captured.width, captured.height), (64, 64))
    assertEquals(captured(16, 20), red)
    assertEquals(captured(0, 0), black)
    intercept[IllegalArgumentException](canvas.snapshot())
  }

  rendering.test("snapshots preserve alpha on transparent native surfaces") {
    case (applet, provider) =>
      provider.initialize(60.0)
      applet.g.clear()
      provider.canvas.noStroke()
      provider.canvas.fill(red.withAlpha(0.5))
      provider.canvas.rect(Vec(0.0, 0.0), Vec(8.0, 8.0))
      val image = provider.canvas.snapshot()
      assertEquals(image(20, 20).alpha, 0.0)
      assertEquals(image(4, 4).red, 1.0)
      assertEqualsDouble(image(4, 4).alpha, 0.5, 1.0 / 255.0)
  }

  rendering.test(
    "images crop, scale and blend without changing drawing state or uploading twice"
  ) { case (_, provider) =>
    provider.initialize(60.0)
    val canvas = provider.canvas
    var reads = 0
    val source = new Image(4, 2):
      protected def read(x: Int, y: Int): Color =
        reads += 1
        if x < 2 then green else red.withAlpha(0.5)
    canvas.background(blue)
    canvas.noStroke()
    canvas.fill(green)
    canvas.translate(Vec(8.0, 8.0))
    canvas.image(source, Vec(0.0, 0.0))
    canvas.image(
      source,
      Vec(8.0, 0.0),
      Vec(16.0, 16.0),
      Vec(2.0, 0.0),
      Vec(2.0, 2.0)
    )
    canvas.rect(Vec(0.0, 24.0), Vec(8.0, 8.0))
    val result = canvas.snapshot()
    assertEquals(reads, 8)
    assertEquals(result(8, 8), green)
    assertEqualsDouble(result(24, 16).red, 0.5, 1.0 / 255.0)
    assertEqualsDouble(result(24, 16).blue, 0.5, 1.0 / 255.0)
    assertEquals(result(4, 16), blue)
    assertEquals(result(12, 36), green)
    provider.resize(80, 48)
    canvas.image(result, Vec(0.0, 0.0))
    assertEquals(canvas.snapshot()(12, 36), green)
  }

  denseRendering.test(
    "snapshot dimensions and coordinates use physical pixels"
  ) { case (_, provider) =>
    provider.initialize(60.0)
    val canvas = provider.canvas
    canvas.background(red)
    canvas.noStroke()
    canvas.fill(blue)
    canvas.rect(Vec(10.0, 10.0), Vec(8.0, 8.0))
    val image = canvas.snapshot()
    assertEquals((canvas.width, canvas.height), (64, 64))
    assertEquals((image.width, image.height), (128, 128))
    assertEquals(image(1, 1), red)
    assertEquals(image(24, 24), blue)
    assertEquals(image(126, 126), red)
    canvas.background(black)
    canvas.image(
      image,
      Vec(0.0, 0.0),
      Vec(16.0, 16.0),
      Vec(20.0, 20.0),
      Vec(16.0, 16.0)
    )
    assertEquals(canvas.snapshot()(16, 16), blue)
  }
