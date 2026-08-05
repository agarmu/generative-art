package genart.processing

import genart.{Color, Frame, Sketch, Step}
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

  rendering.test("initialization establishes portable drawing defaults") {
    case (applet, provider) =>
      applet.g.colorMode(PConstants.HSB, 360.0f, 100.0f, 100.0f, 100.0f)
      applet.g.noFill()
      applet.g.noStroke()
      applet.g.rectMode(PConstants.CENTER)
      provider.initialize(60.0)
      assert(applet.g.fill && applet.g.fillColor == 0xffffffff)
      assert(applet.g.stroke && applet.g.strokeColor == 0xff000000)
      assertEquals(applet.g.strokeWeight, 1.0f)
      provider.canvas.noStroke()
      provider.canvas.fill(red)
      provider.canvas.rect(0.0, 0.0, 8.0, 8.0)
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

  rendering.test(
    "nested scopes restore native style and transforms after failure"
  ) { case (_, provider) =>
    provider.initialize(60.0)
    val canvas = provider.canvas
    canvas.background(black)
    canvas.noStroke()
    canvas.fill(red)
    val result = canvas.scoped {
      canvas.translate(12.0, 0.0)
      canvas.fill(green)
      canvas.rect(0.0, 0.0, 8.0, 8.0)
      canvas.scoped {
        canvas.translate(12.0, 0.0)
        canvas.fill(blue)
        canvas.rect(0.0, 0.0, 8.0, 8.0)
      }
      canvas.rect(0.0, 12.0, 8.0, 8.0)
      42
    }
    assertEquals(result, 42)
    val failure = new RuntimeException("draw")
    val thrown = intercept[RuntimeException] {
      canvas.scoped {
        canvas.translate(40.0, 40.0)
        canvas.noFill()
        canvas.stroke(blue)
        throw failure
      }
    }
    assert(thrown eq failure)
    canvas.rect(0.0, 0.0, 8.0, 8.0)
    canvas.line(48.0, 48.0, 56.0, 48.0)
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
    canvas.rect(0.0, 0.0, 8.0, 8.0)
    canvas.translate(40.0, 40.0)
    provider.beginFrame()
    canvas.rect(0.0, 24.0, 8.0, 8.0)
    val image = canvas.snapshot()
    assertEquals(image(4, 4), red)
    assertEquals(image(4, 28), red)
  }

  rendering.test("native transforms compose around the translated origin") {
    case (_, provider) =>
      provider.initialize(60.0)
      val canvas = provider.canvas
      canvas.background(black)
      canvas.translate(36.0, 12.0)
      canvas.rotate(math.Pi / 2.0)
      canvas.scale(2.0, 1.0)
      canvas.stroke(green)
      canvas.strokeWeight(4.0)
      canvas.line(0.0, 0.0, 8.0, 0.0)
      assertEquals(canvas.snapshot()(36, 20), green)
  }

  rendering.test("background ignores transforms and translucent fills blend") {
    case (_, provider) =>
      provider.initialize(60.0)
      val canvas = provider.canvas
      canvas.translate(10.0, 10.0)
      canvas.background(blue)
      val background = canvas.snapshot()
      assertEquals(background(0, 0), blue)
      assertEquals(background(63, 63), blue)
      provider.beginFrame()
      canvas.noStroke()
      canvas.fill(red.withAlpha(0.5))
      canvas.rect(0.0, 0.0, 8.0, 8.0)
      val mixed = canvas.snapshot()(4, 4)
      assertEqualsDouble(mixed.red, 0.5, 1.0 / 255.0)
      assertEqualsDouble(mixed.blue, 0.5, 1.0 / 255.0)
  }

  rendering.test(
    "native float conversion rejects overflow and zero frame rates"
  ) { case (_, provider) =>
    provider.initialize(60.0)
    intercept[IllegalArgumentException] {
      provider.canvas.translate(Double.MaxValue, 0.0)
    }
    intercept[IllegalArgumentException] {
      provider.canvas.ellipse(0.0, 0.0, Double.MaxValue, 1.0)
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
    canvas.translate(30.0, 30.0)
    provider.resize(80, 48)
    assertEquals((canvas.width, canvas.height), (80, 48))
    assertEquals((applet.width, applet.height), (80, 48))
    assertEquals(canvas.snapshot()(79, 47), black)
    assert(!applet.g.stroke && applet.g.strokeWeight == 4.0f)
    canvas.rect(0.0, 0.0, 8.0, 8.0)
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
      canvas.rect(0.0, 0.0, 8.0, 8.0)
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
              context.canvas.translate(20.0, 20.0)

            override def update(step: Step): Unit = updates += 1

            def draw(frame: Frame): Unit =
              draws += 1
              context.canvas.rect(0.0, 0.0, 8.0, 8.0)

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
    canvas.translate(12.0, 16.0)
    canvas.rect(0.0, 0.0, 8.0, 8.0)
    val captured = canvas.snapshot()
    assertEquals((captured.width, captured.height), (64, 64))
    assertEquals(captured(0, 0), black)
    assertEquals(captured(16, 20), red)
    canvas.rect(12.0, 0.0, 8.0, 8.0)
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
      provider.canvas.rect(0.0, 0.0, 8.0, 8.0)
      val image = provider.canvas.snapshot()
      assertEquals(image(20, 20).alpha, 0.0)
      assertEquals(image(4, 4).red, 1.0)
      assertEqualsDouble(image(4, 4).alpha, 0.5, 1.0 / 255.0)
  }

  denseRendering.test(
    "snapshot dimensions and coordinates use physical pixels"
  ) { case (_, provider) =>
    provider.initialize(60.0)
    val canvas = provider.canvas
    canvas.background(red)
    canvas.noStroke()
    canvas.fill(blue)
    canvas.rect(10.0, 10.0, 8.0, 8.0)
    val image = canvas.snapshot()
    assertEquals((canvas.width, canvas.height), (64, 64))
    assertEquals((image.width, image.height), (128, 128))
    assertEquals(image(1, 1), red)
    assertEquals(image(24, 24), blue)
    assertEquals(image(126, 126), red)
  }
