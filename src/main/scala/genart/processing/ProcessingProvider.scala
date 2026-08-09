package genart.processing

import genart.lib.math.Vec
import genart.{
  Canvas,
  Color,
  Image,
  Path,
  StrokeCap,
  StrokeJoin,
  Sketch,
  SketchContext,
  SketchHandle,
  SketchProvider
}
import genart.lib.runtime.RunConfig
import processing.awt.PGraphicsJava2D
import processing.core.{PApplet, PConstants, PGraphics}
import java.awt.{Component, EventQueue, RenderingHints}
import java.awt.geom.{AffineTransform, Arc2D, Path2D, Rectangle2D}
import java.awt.image.BufferedImage
import java.util.WeakHashMap
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
  private val imageCache = new WeakHashMap[Image, BufferedImage]
  private var renderThread = Option.empty[Thread]

  private def graphics: PGraphics = applet.g

  val canvas: Canvas = new Canvas:
    def width: Int = graphics.width
    def height: Int = graphics.height

    def snapshot(): Image =
      require(!closed, "provider is closed")
      new ProcessingImage(graphics.get().getNative.asInstanceOf[BufferedImage])

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

    protected def setStrokeCap(cap: StrokeCap): Unit =
      graphics.strokeCap(cap match
        case StrokeCap.Butt   => PConstants.SQUARE
        case StrokeCap.Round  => PConstants.ROUND
        case StrokeCap.Square => PConstants.PROJECT)

    protected def setStrokeJoin(join: StrokeJoin): Unit =
      graphics.strokeJoin(join match
        case StrokeJoin.Miter => PConstants.MITER
        case StrokeJoin.Round => PConstants.ROUND
        case StrokeJoin.Bevel => PConstants.BEVEL)

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

    protected def drawPath(path: Path): Unit =
      Path.points(path).foreach { point =>
        native(point.x)
        native(point.y)
      }
      val shape = new Path2D.Double(Path2D.WIND_NON_ZERO)
      shape.moveTo(path.start.x, path.start.y)
      path.segments.foreach {
        case Path.Segment.Line(to)               => shape.lineTo(to.x, to.y)
        case Path.Segment.Quadratic(control, to) =>
          shape.quadTo(control.x, control.y, to.x, to.y)
        case Path.Segment.Cubic(control1, control2, to) =>
          shape.curveTo(
            control1.x,
            control1.y,
            control2.x,
            control2.y,
            to.x,
            to.y
          )
        case Path.Segment.Arc(center, size, start, sweep) =>
          if sweep > 0.0 && size.x > 0.0 && size.y > 0.0 then
            shape.append(
              new Arc2D.Double(
                center.x - size.x / 2.0,
                center.y - size.y / 2.0,
                size.x,
                size.y,
                -math.toDegrees(Path.normalizedAngle(start)),
                -math.toDegrees(sweep),
                Arc2D.OPEN
              ),
              true
            )
      }
      if path.closed then shape.closePath()
      val context = graphics.asInstanceOf[PGraphicsJava2D].g2
      val paint = context.getPaint
      try
        if graphics.fill then
          context.setColor(new java.awt.Color(graphics.fillColor, true))
          context.fill(shape)
        if graphics.stroke then
          context.setColor(new java.awt.Color(graphics.strokeColor, true))
          context.draw(shape)
      finally context.setPaint(paint)

    protected def drawImage(
        source: Image,
        position: Vec[2],
        size: Vec[2],
        sourcePosition: Vec[2],
        sourceSize: Vec[2]
    ): Unit =
      require(!closed, "provider is closed")
      native(position.x)
      native(position.y)
      native(size.x)
      native(size.y)
      val bitmap = source match
        case image: ProcessingImage => image.bitmap
        case _                      =>
          Option(imageCache.get(source)).getOrElse {
            val uploaded = ProcessingImage.upload(source)
            imageCache.put(source, uploaded)
            uploaded
          }
      val drawing = graphics
        .asInstanceOf[PGraphicsJava2D]
        .g2
        .create()
        .asInstanceOf[java.awt.Graphics2D]
      try
        drawing.setRenderingHint(
          RenderingHints.KEY_INTERPOLATION,
          RenderingHints.VALUE_INTERPOLATION_BILINEAR
        )
        drawing.clip(
          new Rectangle2D.Double(position.x, position.y, size.x, size.y)
        )
        val transform = new AffineTransform
        transform.translate(position.x, position.y)
        transform.scale(size.x / sourceSize.x, size.y / sourceSize.y)
        transform.translate(-sourcePosition.x, -sourcePosition.y)
        drawing.drawImage(bitmap, transform, null)
        ()
      finally drawing.dispose()

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
    canvas.strokeCap(StrokeCap.Round)
    canvas.strokeJoin(StrokeJoin.Miter)
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
      imageCache.clear()
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
