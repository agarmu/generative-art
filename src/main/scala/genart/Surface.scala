package genart

/** A transparent, fixed-size offscreen drawing target with independent state.
  * Close it to release native resources and its child surfaces. Captured images
  * survive closing; further canvas operations are rejected. Artistic blend
  * modes on transparent pixels follow each backend's native alpha semantics.
  */
trait Surface extends AutoCloseable:
  def canvas: Canvas
  def isClosed: Boolean
  def close(): Unit

  final def snapshot(): Image = canvas.snapshot()
