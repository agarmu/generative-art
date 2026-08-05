package genart.p5

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

  val canvas: Canvas = new Canvas:
    def width: Int = instance.width
    def height: Int = instance.height

    def snapshot(): Image =
      require(!closed, "provider is closed")
      val captured = instance.drawingContext.getImageData(
        0.0,
        0.0,
        instance.canvas.width,
        instance.canvas.height
      )
      new Image(captured.width, captured.height):
        protected def read(x: Int, y: Int): Color =
          val offset = (y * this.width + x) * 4
          val pixels = captured.data
          Color.rgb(
            pixels(offset) / 255.0,
            pixels(offset + 1) / 255.0,
            pixels(offset + 2) / 255.0,
            pixels(offset + 3) / 255.0
          )

    protected def setStrokeColor(color: Color): Unit =
      instance.stroke(color.red, color.green, color.blue, color.alpha)

    protected def setFillColor(color: Color): Unit =
      instance.fill(color.red, color.green, color.blue, color.alpha)

    protected def disableStroke(): Unit = instance.noStroke()
    protected def disableFill(): Unit = instance.noFill()
    protected def setStrokeWeight(weight: Double): Unit =
      instance.strokeWeight(weight)

    protected def clearSurface(color: Color): Unit =
      instance.background(color.red, color.green, color.blue)

    protected def drawRect(
        x: Double,
        y: Double,
        width: Double,
        height: Double
    ): Unit = instance.rect(x, y, width, height)

    protected def drawLine(
        x1: Double,
        y1: Double,
        x2: Double,
        y2: Double
    ): Unit = instance.line(x1, y1, x2, y2)

    protected def translateBy(x: Double, y: Double): Unit =
      instance.translate(x, y)
    protected def rotateBy(radians: Double): Unit = instance.rotate(radians)
    protected def scaleBy(x: Double, y: Double): Unit = instance.scale(x, y)
    protected def pushState(): Unit = instance.push()
    protected def popState(): Unit = instance.pop()

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
    instance.angleMode(instance.RADIANS)
    instance.stroke(0.0, 0.0, 0.0, 1.0)
    instance.fill(1.0, 1.0, 1.0, 1.0)
    instance.strokeWeight(1.0)
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
      onClose()
