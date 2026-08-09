package genart

import genart.lib.random.Seed

final case class Step(index: Long, seconds: Double, deltaSeconds: Double)
final case class Frame(index: Long, deltaSeconds: Double, interpolation: Double)
final case class SketchContext private[genart] (canvas: Canvas, seed: Seed)

trait Sketch:
  def setup(): Unit = ()
  def update(step: Step): Unit = ()
  def pointer(event: PointerEvent): Unit = ()
  def key(event: KeyEvent): Unit = ()
  def draw(frame: Frame): Unit
  def resized(): Unit = ()
  def dispose(): Unit = ()

trait SketchHandle:
  def stop(): Unit
  def isStopped: Boolean
