package genart.processing

import genart.lib.math.Vec
import genart.{
  Canvas,
  Color,
  Image,
  Sketch,
  SketchContext,
  SketchHandle,
  SketchProvider
}
import genart.lib.runtime.RunConfig
import processing.awt.PGraphicsJava2D
import processing.core.{PApplet, PConstants, PGraphics}
import java.awt.{Component, EventQueue}
import javax.swing.SwingUtilities

object ProcessingProvider:
  final case class Window(
      title: String = "genart",
      size: Option[(Int, Int)] = None,
      resizable: Boolean = true
  ):
    require(title.nonEmpty, "window title must not be empty")
    size.foreach { case (width, height) =>
      require(width > 0 && height > 0, "dimensions must be positive")
    }

  def run(config: RunConfig = RunConfig(), window: Window = Window())(
      create: SketchContext => Sketch
  ): SketchHandle =
    val host = new ProcessingHost(config, create, window)
    PApplet.runSketch(Array("genart"), host)
    new SketchHandle:
      def stop(): Unit = host.requestStop()
      def isStopped: Boolean = host.isStopped

final class ProcessingProvider(applet: PApplet) extends SketchProvider:
  private var closed = false
  private var renderThread = Option.empty[Thread]

  private def graphics: PGraphics = applet.g

  val canvas: Canvas = new Canvas:
    def width: Int = graphics.width
    def height: Int = graphics.height

    def snapshot(): Image =
      require(!closed, "provider is closed")
      val captured = graphics.get()
      val pixels = captured.pixels
      new Image(captured.pixelWidth, captured.pixelHeight):
        protected def read(x: Int, y: Int): Color =
          val argb = pixels(y * this.width + x)
          Color.rgb(
            ((argb >>> 16) & 255) / 255.0,
            ((argb >>> 8) & 255) / 255.0,
            (argb & 255) / 255.0,
            ((argb >>> 24) & 255) / 255.0
          )

    protected def setStrokeColor(color: Color): Unit =
      graphics.stroke(
        color.red.toFloat,
        color.green.toFloat,
        color.blue.toFloat,
        color.alpha.toFloat
      )

    protected def setFillColor(color: Color): Unit =
      graphics.fill(
        color.red.toFloat,
        color.green.toFloat,
        color.blue.toFloat,
        color.alpha.toFloat
      )

    protected def disableStroke(): Unit = graphics.noStroke()
    protected def disableFill(): Unit = graphics.noFill()
    protected def setStrokeWeight(weight: Double): Unit =
      graphics.strokeWeight(native(weight))

    protected def clearSurface(color: Color): Unit =
      graphics.background(
        color.red.toFloat,
        color.green.toFloat,
        color.blue.toFloat
      )

    protected def drawRect(position: Vec[2], size: Vec[2]): Unit =
      graphics.rect(
        native(position.x),
        native(position.y),
        native(size.x),
        native(size.y)
      )

    protected def drawLine(from: Vec[2], to: Vec[2]): Unit =
      graphics.line(native(from.x), native(from.y), native(to.x), native(to.y))

    protected def drawEllipse(center: Vec[2], size: Vec[2]): Unit =
      graphics.ellipse(
        native(center.x),
        native(center.y),
        native(size.x),
        native(size.y)
      )

    protected def drawTriangle(a: Vec[2], b: Vec[2], c: Vec[2]): Unit =
      graphics.triangle(
        native(a.x),
        native(a.y),
        native(b.x),
        native(b.y),
        native(c.x),
        native(c.y)
      )

    protected def drawQuad(a: Vec[2], b: Vec[2], c: Vec[2], d: Vec[2]): Unit =
      graphics.quad(
        native(a.x),
        native(a.y),
        native(b.x),
        native(b.y),
        native(c.x),
        native(c.y),
        native(d.x),
        native(d.y)
      )

    protected def translateBy(offset: Vec[2]): Unit =
      graphics.translate(native(offset.x), native(offset.y))

    protected def rotateBy(radians: Double): Unit =
      graphics.rotate(native(radians))
    protected def scaleBy(factors: Vec[2]): Unit =
      graphics.scale(native(factors.x), native(factors.y))

    protected def pushState(): Unit =
      graphics.pushStyle()
      try graphics.pushMatrix()
      catch
        case error: Throwable =>
          graphics.popStyle()
          throw error

    protected def popState(): Unit =
      try graphics.popStyle()
      finally graphics.popMatrix()

  def initialize(frameRate: Double): Unit =
    require(!closed, "provider is closed")
    renderThread = Some(Thread.currentThread())
    require(
      graphics != null,
      "Processing graphics must exist before initialization"
    )
    require(
      graphics.isInstanceOf[PGraphicsJava2D],
      "provider requires the Java2D renderer"
    )
    val rate = native(frameRate)
    require(rate > 0.0f, "frameRate must be positive at native precision")
    applet.frameRate(rate)
    graphics.colorMode(PConstants.RGB, 1.0f)
    graphics.blendMode(PConstants.BLEND)
    graphics.rectMode(PConstants.CORNER)
    graphics.ellipseMode(PConstants.CENTER)
    graphics.stroke(0.0f, 0.0f, 0.0f, 1.0f)
    graphics.fill(1.0f, 1.0f, 1.0f, 1.0f)
    graphics.strokeWeight(1.0f)
    graphics.resetMatrix()

  def beginFrame(): Unit = graphics.resetMatrix()

  def resize(width: Int, height: Int): Unit =
    require(width > 0 && height > 0, "dimensions must be positive")
    val style = graphics.getStyle()
    if graphics.width != width || graphics.height != height then
      graphics.endDraw()
      applet.getSurface.setSize(width, height)
      graphics.setSize(width, height)
      graphics.beginDraw()
    graphics.resetMatrix()
    graphics.background(0.0f, 0.0f, 0.0f)
    graphics.style(style)

  def close(): Unit =
    if !closed then
      closed = true
      val surface = applet.getSurface
      if surface != null then
        applet.noLoop()
        surface.setFrameRate(60.0f)
        try applet.dispose()
        finally
          surface.getNative match
            case component: Component =>
              val releaseWindow: Runnable = () =>
                renderThread.foreach(_.join())
                EventQueue.invokeLater(() =>
                  Option(SwingUtilities.getWindowAncestor(component))
                    .foreach(_.dispose())
                )
              val teardown =
                new Thread(releaseWindow, "genart-window-disposal")
              teardown.start()
            case _ => ()

  private def native(value: Double): Float =
    val converted = value.toFloat
    require(converted.isFinite, "value exceeds Processing's finite Float range")
    converted
