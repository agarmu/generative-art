package genart.processing

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

    protected def drawRect(
        x: Double,
        y: Double,
        width: Double,
        height: Double
    ): Unit = graphics.rect(native(x), native(y), native(width), native(height))

    protected def drawLine(
        x1: Double,
        y1: Double,
        x2: Double,
        y2: Double
    ): Unit = graphics.line(native(x1), native(y1), native(x2), native(y2))

    protected def drawEllipse(
        centerX: Double,
        centerY: Double,
        width: Double,
        height: Double
    ): Unit =
      graphics.ellipse(
        native(centerX),
        native(centerY),
        native(width),
        native(height)
      )

    protected def translateBy(x: Double, y: Double): Unit =
      graphics.translate(native(x), native(y))

    protected def rotateBy(radians: Double): Unit =
      graphics.rotate(native(radians))
    protected def scaleBy(x: Double, y: Double): Unit =
      graphics.scale(native(x), native(y))

    protected def pushState(): Unit = graphics.push()
    protected def popState(): Unit = graphics.pop()

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
