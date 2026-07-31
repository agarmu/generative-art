import processing.core.PApplet
import processing.core.PApplet.*
import processing.core.PConstants.*
import javax.imageio.ImageIO
import java.io.File

object Palette:
  var colors: Array[(Float, Float, Float)] = defaultPalette

  def load(path: String, n: Int = 800): Unit =
    try
      val img = ImageIO.read(File(path))
      val rng = scala.util.Random
      colors = Array.fill(n) {
        val rgb =
          img.getRGB(rng.nextInt(img.getWidth), rng.nextInt(img.getHeight))
        val hsb = java.awt.Color.RGBtoHSB(
          (rgb >> 16) & 0xff,
          (rgb >> 8) & 0xff,
          rgb & 0xff,
          null
        )
        (hsb(0) * 360f, hsb(1) * 100f, hsb(2) * 100f)
      }
    catch
      case e: Exception =>
        println(
          s"Could not load '$path': ${e.getMessage}, using default palette"
        )

  private def defaultPalette: Array[(Float, Float, Float)] = Array(
    (112f, 72f, 10f),
    (115f, 68f, 18f),
    (108f, 65f, 26f),
    (95f, 65f, 36f),
    (78f, 72f, 52f),
    (50f, 82f, 74f),
    (44f, 78f, 84f),
    (46f, 28f, 92f),
    (40f, 12f, 97f)
  )

class Sketch extends PApplet:

  val NoiseScale = 0.004f

  // all sizing derived from screen at setup time
  var cellSize, numLines: Int = _
  var swMin, swMax: Float = _

  var palette: Array[(Float, Float, Float)] = _
  var bgH, bgS, bgB: Float = _
  var occupied: Array[Boolean] = _
  var angles: Array[Float] = _
  var cols, rows: Int = _
  val MaxDensityMult = 5.0f
  val GrowthRate = 1.006f
  val MinGrowthAmount = 5f
  val MaxGrowthAmount = 25f
  var maxLines = 0
  var startLines = 0
  var currentBatch = 0f
  var drawn = 0
  var frameN = 0
  val FadeEvery = 10 // apply 1% fade every N frames → effective ~0.2% per frame

  override def settings(): Unit = fullScreen()

  override def setup(): Unit =
    colorMode(HSB, 360f, 100f, 100f, 100f)
    frameRate(60)

    val scale = math.min(width, height) / 1080f
    cellSize = math.max(2, (7 * scale).round.toInt)
    swMin = 1.5f * scale
    swMax = 4.0f * scale

    palette = Palette.colors
    val (h, s, b) = palette.minBy(_._3)
    bgH = h; bgS = s; bgB = b * 0.6f

    cols = width / cellSize
    rows = height / cellSize
    occupied = new Array[Boolean](cols * rows)
    angles = new Array[Float](cols * rows)

    maxLines = (cols * rows * MaxDensityMult).toInt
    startLines = math.max(1, (maxLines * 0.005f).toInt)

    noiseSeed(random(100000f).toInt)
    recomputeAngles()
    drawn = 0

  override def draw(): Unit =
    if drawn == 0 then
      background(bgH, bgS, bgB)
      java.util.Arrays.fill(occupied, false)
      currentBatch = 10f

    if currentBatch % FadeEvery == 0 then
      noStroke()
      fill(bgH, bgS, bgB, 8f)
      rect(0, 0, width.toFloat, height.toFloat)

    val batch = math.min(currentBatch.toInt, maxLines - drawn)
    for _ <- 0 until batch do drawLine()
    drawn += batch
    currentBatch = math.max(
      math.min(currentBatch * GrowthRate, currentBatch + MaxGrowthAmount),
      currentBatch + MinGrowthAmount
    )

    if drawn >= maxLines then
      noiseSeed(random(100000f).toInt)
      recomputeAngles()
      drawn = 0

  private def recomputeAngles(): Unit =
    for c <- 0 until cols; r <- 0 until rows do
      angles(r * cols + c) =
        noise(c * NoiseScale, r * NoiseScale) * TWO_PI * 2.5f

  private def isOccupied(c: Int, r: Int): Boolean =
    c < 0 || c >= cols || r < 0 || r >= rows || occupied(r * cols + c)

  private def drawLine(): Unit =
    var x = random(width)
    var y = random(height)
    val steps = random(4f, 22f).toInt
    val (h, s, b) = palette(random(palette.length.toFloat).toInt)

    strokeWeight(random(swMin, swMax))
    stroke(h, s, b, 92f)
    noFill()

    var i = 0
    while i < steps do
      val c = (x / cellSize).toInt
      val r = (y / cellSize).toInt
      if isOccupied(c, r) then i = steps
      else
        occupied(r * cols + c) = true
        val a = angles(r * cols + c)
        val nx = x + cos(a) * cellSize
        val ny = y + sin(a) * cellSize
        line(x, y, nx, ny)
        x = nx
        y = ny
        i += 1

object Sketch:
  def main(args: Array[String]): Unit =
    Palette.load(args.headOption.getOrElse("palette.png"))
    PApplet.main(Array("Sketch") ++ args)
