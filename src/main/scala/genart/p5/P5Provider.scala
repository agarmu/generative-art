package genart.p5

import genart.lib.math.Vec
import genart.{
  Canvas,
  BlendMode,
  Gradient,
  Surface,
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
import org.scalajs.dom
import scala.scalajs.js
import scala.util.Using

object P5Provider:
  def run(container: dom.Element, config: RunConfig = RunConfig())(
      create: SketchContext => Sketch
  ): SketchHandle =
    val (width, height) = dimensions(container)
    require(
      width > 0 && height > 0,
      "sketch container must have positive dimensions"
    )
    val host = new P5Host(container, config, create)
    host.start()
    host

  private[p5] def dimensions(container: dom.Element): (Int, Int) =
    def pixels(value: String): Double = value.stripSuffix("px").toDouble
    val style = dom.window.getComputedStyle(container)
    val width = container.clientWidth - pixels(style.paddingLeft) -
      pixels(style.paddingRight)
    val height = container.clientHeight - pixels(style.paddingTop) -
      pixels(style.paddingBottom)
    (width.toInt, height.toInt)

final class P5Provider private[p5] (
    host: P5,
    container: dom.Element,
    onClose: () => Unit,
    target: Option[P5Graphics] = None
) extends SketchProvider:
  private def instance: P5Drawing = target.getOrElse(host)
  private var closed = false
  private val children = scala.collection.mutable.Set.empty[Surface]
  private var imageCache = new ImageCache
  private var fillEnabled = true
  private var strokeEnabled = true
  private var savedPaint = List.empty[(Boolean, Boolean)]

  val canvas: Canvas = new Canvas:
    def width: Int =
      ensureOpen()
      instance.width
    def height: Int =
      ensureOpen()
      instance.height

    def snapshot(): Image =
      require(!closed, "provider is closed")
      P5Image.capture(instance.canvas)

    override protected def ensureOpen(): Unit =
      require(!closed, "provider is closed")

    protected def clearTransparent(): Unit =
      val context = instance.drawingContext
      context.save()
      try
        context.setTransform(1, 0, 0, 1, 0, 0)
        context.clearRect(0, 0, width, height)
      finally context.restore()

    private def css(color: Color): String =
      s"rgba(${color.red * 255},${color.green * 255},${color.blue * 255},${color.alpha})"

    protected def createSurface(width: Int, height: Int): Surface =
      val graphics = host.createGraphics(width, height)
      var registered = Option.empty[Surface]
      val provider = new P5Provider(
        host,
        container,
        () => {
          try graphics.remove()
          finally registered.foreach(children -= _)
        },
        Some(graphics)
      )
      val surface = new Surface:
        def canvas: Canvas = provider.canvas
        def isClosed: Boolean = provider.closed
        def close(): Unit = provider.close()
      registered = Some(surface)
      try provider.initialize(0.0)
      catch
        case failure: Throwable => Using.resource(surface)(_ => throw failure)
      children += surface
      surface

    protected def setStrokeColor(color: Color): Unit =
      instance.stroke(color.red, color.green, color.blue, color.alpha)
      strokeEnabled = true

    protected def setFillColor(color: Color): Unit =
      instance.fill(color.red, color.green, color.blue, color.alpha)
      fillEnabled = true

    protected def disableStroke(): Unit =
      instance.noStroke()
      strokeEnabled = false
    protected def disableFill(): Unit =
      instance.noFill()
      fillEnabled = false
    protected def setStrokeWeight(weight: Double): Unit =
      instance.strokeWeight(weight)

    protected def setStrokeCap(cap: StrokeCap): Unit =
      instance.strokeCap(cap match
        case StrokeCap.Butt   => host.SQUARE
        case StrokeCap.Round  => host.ROUND
        case StrokeCap.Square => host.PROJECT)

    protected def setStrokeJoin(join: StrokeJoin): Unit =
      instance.strokeJoin(join match
        case StrokeJoin.Miter => host.MITER
        case StrokeJoin.Round => host.ROUND
        case StrokeJoin.Bevel => host.BEVEL)

    protected def clearSurface(color: Color): Unit =
      val context = instance.drawingContext
      context.save()
      try
        context.setTransform(1, 0, 0, 1, 0, 0)
        context.globalCompositeOperation = "source-over"
        context.fillStyle = css(Color.rgb(color.red, color.green, color.blue))
        context.fillRect(0, 0, width, height)
      finally context.restore()

    protected def drawRect(position: Vec[2], size: Vec[2]): Unit =
      instance.rect(position.x, position.y, size.x, size.y)

    protected def drawLine(from: Vec[2], to: Vec[2]): Unit =
      instance.line(from.x, from.y, to.x, to.y)

    private def font(style: TextStyle): String =
      val family = style.family match
        case FontFamily.SansSerif => "sans-serif"
        case FontFamily.Serif     => "serif"
        case FontFamily.Monospace => "monospace"
      s"${if style.italic then "italic" else "normal"} ${
          if style.bold then "bold" else "normal"
        } ${style.size}px $family"

    protected def measureTextLine(
        value: String,
        style: TextStyle
    ): TextMetrics =
      val context = instance.drawingContext
      context.save()
      try
        context.font = font(style)
        context.textAlign = "left"
        context.textBaseline = "alphabetic"
        val measured =
          context.measureText(value).asInstanceOf[NativeTextMetrics]
        val reference =
          context.measureText("Mg").asInstanceOf[NativeTextMetrics]
        val ascent = measured.fontBoundingBoxAscent.getOrElse(
          reference.actualBoundingBoxAscent
        )
        val descent = measured.fontBoundingBoxDescent.getOrElse(
          reference.actualBoundingBoxDescent
        )
        TextMetrics(measured.width, ascent + descent, ascent, descent)
      finally context.restore()

    protected def drawTextLine(
        value: String,
        position: Vec[2],
        style: TextStyle
    ): Unit =
      val context = instance.drawingContext
      context.save()
      try
        context.font = font(style)
        context.textAlign = "left"
        context.textBaseline = "alphabetic"
        if fillEnabled then context.fillText(value, position.x, position.y)
        if strokeEnabled then context.strokeText(value, position.x, position.y)
      finally context.restore()

    protected def drawEllipse(center: Vec[2], size: Vec[2]): Unit =
      instance.ellipse(center.x, center.y, size.x, size.y)

    protected def drawTriangle(a: Vec[2], b: Vec[2], c: Vec[2]): Unit =
      instance.triangle(a.x, a.y, b.x, b.y, c.x, c.y)

    protected def drawQuad(a: Vec[2], b: Vec[2], c: Vec[2], d: Vec[2]): Unit =
      instance.quad(a.x, a.y, b.x, b.y, c.x, c.y, d.x, d.y)

    protected def translateBy(offset: Vec[2]): Unit =
      instance.translate(offset.x, offset.y)

    private def shape(path: Path): dom.Path2D =
      val shape = new dom.Path2D()
      shape.moveTo(path.start.x, path.start.y)
      path.segments.foreach {
        case Path.Segment.Move(to)               => shape.moveTo(to.x, to.y)
        case Path.Segment.Close                  => shape.closePath()
        case Path.Segment.Line(to)               => shape.lineTo(to.x, to.y)
        case Path.Segment.Quadratic(control, to) =>
          shape.quadraticCurveTo(control.x, control.y, to.x, to.y)
        case Path.Segment.Cubic(control1, control2, to) =>
          shape.bezierCurveTo(
            control1.x,
            control1.y,
            control2.x,
            control2.y,
            to.x,
            to.y
          )
        case Path.Segment.Arc(center, size, start, sweep) =>
          if sweep != 0.0 && size.x > 0.0 && size.y > 0.0 then
            val angle = Path.normalizedAngle(start)
            shape.ellipse(
              center.x,
              center.y,
              size.x / 2.0,
              size.y / 2.0,
              0.0,
              angle,
              angle + sweep,
              sweep < 0.0
            )
      }
      shape

    private def winding(rule: FillRule): dom.CanvasFillRule = rule match
      case FillRule.NonZero => dom.CanvasFillRule.nonzero
      case FillRule.EvenOdd => dom.CanvasFillRule.evenodd

    protected def drawPath(path: Path, rule: FillRule): Unit =
      val outline = shape(path)
      if fillEnabled then instance.drawingContext.fill(outline, winding(rule))
      if strokeEnabled then instance.drawingContext.stroke(outline)

    protected def intersectClip(path: Path, rule: FillRule): Unit =
      instance.drawingContext.clip(shape(path), winding(rule))

    protected def drawImage(
        source: Image,
        position: Vec[2],
        size: Vec[2],
        sourcePosition: Vec[2],
        sourceSize: Vec[2]
    ): Unit =
      require(!closed, "provider is closed")
      val surface = source match
        case image: P5Image => image.surface
        case _              =>
          imageCache.get(source).getOrElse {
            val uploaded = P5Image.upload(source)
            imageCache.set(source, uploaded)
            uploaded
          }
      val drawing = instance.drawingContext
      drawing.save()
      try
        drawing.imageSmoothingEnabled = true
        drawing.drawImage(
          surface,
          sourcePosition.x,
          sourcePosition.y,
          sourceSize.x,
          sourceSize.y,
          position.x,
          position.y,
          size.x,
          size.y
        )
      finally drawing.restore()

    protected def rotateBy(radians: Double): Unit = instance.rotate(radians)
    protected def scaleBy(factors: Vec[2]): Unit =
      instance.scale(factors.x, factors.y)
    protected def pushState(): Unit =
      instance.push()
      savedPaint = (fillEnabled, strokeEnabled) :: savedPaint

    protected def popState(): Unit =
      try
        if !closed then instance.pop()
      finally
        val (fill, stroke) = savedPaint.head
        savedPaint = savedPaint.tail
        if !closed then
          fillEnabled = fill
          strokeEnabled = stroke

  def initialize(frameRate: Double): Unit =
    require(!closed, "provider is closed")
    if target.isEmpty then
      host.canvas.style.display = "none"
      val (width, height) = P5Provider.dimensions(container)
      require(
        width > 0 && height > 0,
        "sketch container must have positive dimensions"
      )
      host.createCanvas(width, height)
      host.canvas.style.display = "block"
      host.frameRate(frameRate)
    instance.pixelDensity(1)
    instance.colorMode(host.RGB, 1.0)
    instance.blendMode(host.BLEND)
    instance.rectMode(host.CORNER)
    instance.ellipseMode(host.CENTER)
    instance.angleMode(host.RADIANS)
    instance.stroke(0.0, 0.0, 0.0, 1.0)
    instance.fill(1.0, 1.0, 1.0, 1.0)
    instance.strokeWeight(1.0)
    canvas.strokeCap(StrokeCap.Round)
    canvas.strokeJoin(StrokeJoin.Miter)
    instance.resetMatrix()
    if target.nonEmpty then canvas.clear()

  def beginFrame(): Unit = instance.resetMatrix()

  def resize(width: Int, height: Int): Unit =
    require(width > 0 && height > 0, "dimensions must be positive")
    instance.resizeCanvas(width, height, true)
    instance.resetMatrix()
    canvas.background(Color.rgb(0.0, 0.0, 0.0))

  def close(): Unit =
    if !closed then
      closed = true
      try
        Using.Manager { use =>
          use(
            new AutoCloseable:
              def close(): Unit = onClose()
          )
          children.toList.foreach(child => use(child))
        }.get
      finally
        children.clear()
        imageCache = new ImageCache

@js.native
private[p5] trait NativeTextMetrics extends js.Object:
  val width: Double = js.native
  val fontBoundingBoxAscent: js.UndefOr[Double] = js.native
  val fontBoundingBoxDescent: js.UndefOr[Double] = js.native
  val actualBoundingBoxAscent: Double = js.native
  val actualBoundingBoxDescent: Double = js.native
