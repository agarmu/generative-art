package genart

final case class Step(index: Long, seconds: Double, deltaSeconds: Double)
final case class Frame(index: Long, deltaSeconds: Double, interpolation: Double)

trait Sketch:
  def setup(): Unit = ()
  def draw(): Unit
  def resized(): Unit = ()
  def dispose(): Unit = ()
