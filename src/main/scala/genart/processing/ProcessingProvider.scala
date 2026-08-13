package genart.processing

import genart.lib.math.Vec
import genart.{
  Canvas,
  Surface,
  BlendMode,
  Gradient,
  Color,
  Image,
  Path,
  FillRule,
  FontFamily,
  TextMetrics,
  TextStyle,
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
import java.awt.{AlphaComposite, Component, EventQueue, RenderingHints}
import java.awt.image.RescaleOp
import scala.util.Using
import java.awt.geom.{
  AffineTransform,
  Arc2D,
  Ellipse2D,
  Line2D,
  Path2D,
  Point2D,
  Rectangle2D
}
import java.awt.image.BufferedImage
import java.awt.font.TextLayout
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

final class ProcessingProvider private (
    applet: PApplet,
    target: Option[PGraphicsJava2D]
) extends SketchProvider:
  def this(applet: PApplet) = this(applet, None)
  private var closed = false
  private val children = scala.collection.mutable.Set.empty[Surface]
  private val imageCache = new WeakHashMap[Image, BufferedImage]
  private var savedClips = List.empty[java.awt.Shape]
  private var renderThread = Option.empty[Thread]

  private def graphics: PGraphics = target.getOrElse(applet.g)
  private def context = graphics.asInstanceOf[PGraphicsJava2D].g2

  val canvas: Canvas = new Canvas:
    def width: Int =
      ensureOpen()
      graphics.width
    def height: Int =
      ensureOpen()
      graphics.height

    def snapshot(): Image =
      require(!closed, "provider is closed")
      ProcessingImage.from(
        graphics.asInstanceOf[PGraphicsJava2D].image.asInstanceOf[BufferedImage]
      )

    override protected def ensureOpen(): Unit =
      require(!closed, "provider is closed")

    private def clearWith(color: Option[Color]): Unit =
      val drawing = context.create().asInstanceOf[java.awt.Graphics2D]
      try
        drawing.setTransform(new AffineTransform)
        drawing.setComposite(AlphaComposite.Src)
        drawing.setColor(
          color.fold(new java.awt.Color(0, 0, 0, 0))(c =>
            new java.awt.Color(c.red.toFloat, c.green.toFloat, c.blue.toFloat)
          )
        )
        drawing.fillRect(0, 0, graphics.pixelWidth, graphics.pixelHeight)
      finally drawing.dispose()

    protected def clearTransparent(): Unit = clearWith(None)

    protected def createSurface(width: Int, height: Int): Surface =
      val layer = applet
        .createGraphics(width, height, PConstants.JAVA2D)
        .asInstanceOf[PGraphicsJava2D]
      val provider = new ProcessingProvider(applet, Some(layer))
      try
        layer.pixelDensity = 1
        layer.setSize(width, height)
        layer.beginDraw()
        provider.initialize(60.0)
        provider.canvas.clear()
      catch
        case error: Throwable =>
          try provider.close()
          catch case cleanup: Throwable => error.addSuppressed(cleanup)
          throw error
      val surface = new Surface:
        val canvas: Canvas = provider.canvas
        def isClosed: Boolean = provider.closed
        def close(): Unit =
          try provider.close()
          finally children -= this
      children += surface
      surface

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

    protected def clearSurface(color: Color): Unit = clearWith(Some(color))
    protected def drawRect(position: Vec[2], size: Vec[2]): Unit =
      graphics.rect(
        native(position.x),
        native(position.y),
        native(size.x),
        native(size.y)
      )

    protected def drawLine(from: Vec[2], to: Vec[2]): Unit =
      graphics.line(native(from.x), native(from.y), native(to.x), native(to.y))

    private def layout(value: String, style: TextStyle): TextLayout =
      val family = style.family match
        case FontFamily.SansSerif => java.awt.Font.SANS_SERIF
        case FontFamily.Serif     => java.awt.Font.SERIF
        case FontFamily.Monospace => java.awt.Font.MONOSPACED
      val flags =
        (if style.bold then java.awt.Font.BOLD else java.awt.Font.PLAIN) |
          (if style.italic then java.awt.Font.ITALIC else java.awt.Font.PLAIN)
      val size = native(style.size)
      require(size > 0.0f, "text size must be positive at native precision")
      val font = new java.awt.Font(family, flags, 1).deriveFont(size)
      val context = graphics.asInstanceOf[PGraphicsJava2D].g2
      new TextLayout(
        if value.isEmpty then " " else value,
        font,
        context.getFontRenderContext
      )

    protected def measureTextLine(
        value: String,
        style: TextStyle
    ): TextMetrics =
      val measured = layout(value, style)
      val ascent = measured.getAscent.toDouble
      val descent = measured.getDescent.toDouble
      TextMetrics(
        if value.isEmpty then 0.0 else measured.getAdvance.toDouble,
        ascent + descent,
        ascent,
        descent
      )

    protected def drawTextLine(
        value: String,
        position: Vec[2],
        style: TextStyle
    ): Unit =
      native(position.x)
      native(position.y)
      val shape = layout(value, style).getOutline(
        AffineTransform.getTranslateInstance(position.x, position.y)
      )
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

    private def shape(path: Path, rule: FillRule): Path2D.Double =
      Path.points(path).foreach { point =>
        native(point.x)
        native(point.y)
      }
      val winding = rule match
        case FillRule.NonZero => Path2D.WIND_NON_ZERO
        case FillRule.EvenOdd => Path2D.WIND_EVEN_ODD
      val shape = new Path2D.Double(winding)
      shape.moveTo(path.start.x, path.start.y)
      path.segments.foreach {
        case Path.Segment.Move(to)               => shape.moveTo(to.x, to.y)
        case Path.Segment.Close                  => shape.closePath()
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
          if sweep != 0.0 && size.x > 0.0 && size.y > 0.0 then
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
      shape

    protected def intersectClip(path: Path, rule: FillRule): Unit =
      graphics.asInstanceOf[PGraphicsJava2D].g2.clip(shape(path, rule))

    protected def drawPath(path: Path, rule: FillRule): Unit =
      val outline = shape(path, rule)
      val context = graphics.asInstanceOf[PGraphicsJava2D].g2
      val paint = context.getPaint
      try
        if graphics.fill then
          context.setColor(new java.awt.Color(graphics.fillColor, true))
          context.fill(outline)
        if graphics.stroke then
          context.setColor(new java.awt.Color(graphics.strokeColor, true))
          context.draw(outline)
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

    private def withDeviceCoordinates[A](body: java.awt.Graphics2D => A): A =
      val context = graphics.asInstanceOf[PGraphicsJava2D].g2
      val transform = context.getTransform
      // getClip cannot recover user coordinates under a singular transform.
      context.setTransform(new AffineTransform)
      try body(context)
      finally context.setTransform(transform)

    protected def pushState(): Unit =
      val clip = withDeviceCoordinates(_.getClip)
      graphics.pushStyle()
      try graphics.pushMatrix()
      catch
        case error: Throwable =>
          graphics.popStyle()
          throw error
      savedClips = clip :: savedClips

    protected def popState(): Unit =
      val clip = savedClips.head
      savedClips = savedClips.tail
      if !closed then
        try graphics.popStyle()
        finally
          try graphics.popMatrix()
          finally withDeviceCoordinates(_.setClip(clip))

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
    if target.isEmpty then applet.frameRate(rate)
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
      Using.Manager { use =>
        val cleanup = new AutoCloseable:
          def close(): Unit = target match
            case Some(layer) =>
              Using.Manager { own =>
                own(
                  new AutoCloseable:
                    def close(): Unit = layer.dispose()
                )
                layer.endDraw()
              }.get
            case None => closeWindow()
        use(cleanup)
        children.toList.foreach(use(_))
      }.get

  private def closeWindow(): Unit =
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
