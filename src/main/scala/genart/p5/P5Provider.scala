package genart.p5

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
import org.scalajs.dom

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
    instance: P5,
    container: dom.Element,
    onClose: () => Unit
) extends SketchProvider:
  private var closed = false
  private var imageCache = new ImageCache
  private var fillEnabled = true
  private var strokeEnabled = true
  private var savedPaint = List.empty[(Boolean, Boolean)]

  val canvas: Canvas = new Canvas:
    def width: Int = instance.width
    def height: Int = instance.height

    def snapshot(): Image =
      require(!closed, "provider is closed")
      P5Image.capture(instance.canvas)

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
        case StrokeCap.Butt   => instance.SQUARE
        case StrokeCap.Round  => instance.ROUND
        case StrokeCap.Square => instance.PROJECT)

    protected def setStrokeJoin(join: StrokeJoin): Unit =
      instance.strokeJoin(join match
        case StrokeJoin.Miter => instance.MITER
        case StrokeJoin.Round => instance.ROUND
        case StrokeJoin.Bevel => instance.BEVEL)

    protected def clearSurface(color: Color): Unit =
      instance.background(color.red, color.green, color.blue)

    protected def drawRect(position: Vec[2], size: Vec[2]): Unit =
      instance.rect(position.x, position.y, size.x, size.y)

    protected def drawLine(from: Vec[2], to: Vec[2]): Unit =
      instance.line(from.x, from.y, to.x, to.y)

    protected def drawEllipse(center: Vec[2], size: Vec[2]): Unit =
      instance.ellipse(center.x, center.y, size.x, size.y)

    protected def drawTriangle(a: Vec[2], b: Vec[2], c: Vec[2]): Unit =
      instance.triangle(a.x, a.y, b.x, b.y, c.x, c.y)

    protected def drawQuad(a: Vec[2], b: Vec[2], c: Vec[2], d: Vec[2]): Unit =
      instance.quad(a.x, a.y, b.x, b.y, c.x, c.y, d.x, d.y)

    protected def translateBy(offset: Vec[2]): Unit =
      instance.translate(offset.x, offset.y)

    protected def drawPath(path: Path): Unit =
      val shape = new dom.Path2D()
      shape.moveTo(path.start.x, path.start.y)
      path.segments.foreach {
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
          if sweep > 0.0 && size.x > 0.0 && size.y > 0.0 then
            val angle = Path.normalizedAngle(start)
            shape.ellipse(
              center.x,
              center.y,
              size.x / 2.0,
              size.y / 2.0,
              0.0,
              angle,
              angle + sweep
            )
      }
      if path.closed then shape.closePath()
      if fillEnabled then instance.drawingContext.fill(shape)
      if strokeEnabled then instance.drawingContext.stroke(shape)

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
      try instance.pop()
      finally
        val (fill, stroke) = savedPaint.head
        savedPaint = savedPaint.tail
        fillEnabled = fill
        strokeEnabled = stroke

  def initialize(frameRate: Double): Unit =
    require(!closed, "provider is closed")
    instance.canvas.style.display = "none"
    val (width, height) = P5Provider.dimensions(container)
    require(
      width > 0 && height > 0,
      "sketch container must have positive dimensions"
    )
    instance.createCanvas(width, height)
    instance.canvas.style.display = "block"
    instance.pixelDensity(1)
    instance.frameRate(frameRate)
    instance.colorMode(instance.RGB, 1.0)
    instance.blendMode(instance.BLEND)
    instance.rectMode(instance.CORNER)
    instance.ellipseMode(instance.CENTER)
    instance.angleMode(instance.RADIANS)
    instance.stroke(0.0, 0.0, 0.0, 1.0)
    instance.fill(1.0, 1.0, 1.0, 1.0)
    instance.strokeWeight(1.0)
    canvas.strokeCap(StrokeCap.Round)
    canvas.strokeJoin(StrokeJoin.Miter)
    instance.resetMatrix()

  def beginFrame(): Unit = instance.resetMatrix()

  def resize(width: Int, height: Int): Unit =
    require(width > 0 && height > 0, "dimensions must be positive")
    instance.resizeCanvas(width, height, true)
    instance.resetMatrix()
    instance.background(0.0, 0.0, 0.0)

  def close(): Unit =
    if !closed then
      closed = true
      imageCache = new ImageCache
      onClose()
