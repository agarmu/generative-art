package genart

trait SketchProvider:
  val canvas: Canvas
  def initialize(frameRate: Double): Unit
  def beginFrame(): Unit
  def resize(width: Int, height: Int): Unit
  def close(): Unit

  private var claimed = false
  private[genart] final def claim(): Unit =
    require(!claimed, "provider already belongs to a session")
    claimed = true
