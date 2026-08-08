package examples

import genart.{Color, Frame, PointerButton, PointerEvent, Sketch, SketchContext}
import genart.lib.math.{Vec, clamp}

final class DragShapes(context: SketchContext) extends Sketch:
  private val canvas = context.canvas
  private var centers = Vector.empty[Vec[2]]
  private var dragging = Option.empty[(Int, Vec[2])]

  private def side: Double = math.min(canvas.width, canvas.height) * 0.3

  private def bounded(center: Vec[2]): Vec[2] =
    val radius = side / 2.0
    Vec(
      clamp(center.x, radius, canvas.width - radius),
      clamp(center.y, radius, canvas.height - radius)
    )

  override def setup(): Unit =
    centers = Vector(
      Vec(canvas.width * 0.4, canvas.height * 0.5),
      Vec(canvas.width * 0.6, canvas.height * 0.5)
    )

  override def pointer(event: PointerEvent): Unit = event match
    case PointerEvent.Pressed(at, PointerButton.Primary) =>
      dragging = centers.indices.reverse.find { index =>
        val delta = at - centers(index)
        if index == 0 then delta.x.abs <= side / 2.0 && delta.y.abs <= side / 2.0
        else delta.length <= side / 2.0
      }.map(index => (index, at - centers(index)))
    case PointerEvent.Moved(at) =>
      dragging.foreach { case (index, offset) =>
        centers = centers.updated(index, bounded(at - offset))
      }
    case PointerEvent.Released(_, PointerButton.Primary) | PointerEvent.Cancelled =>
      dragging = None
    case _ => ()

  override def resized(): Unit =
    centers = centers.map(bounded)
    dragging = None

  def draw(frame: Frame): Unit =
    canvas.background(Color.rgb(1.0, 1.0, 1.0))
    canvas.stroke(Color.rgb(0.15, 0.15, 0.15))
    for index <- centers.indices do
      canvas.strokeWeight(if dragging.exists(_._1 == index) then 4.0 else 2.0)
      canvas.fill(Color.hsb(if index == 0 then 205.0 else 330.0, 0.6, 0.85, 0.75))
      if index == 0 then
        canvas.rect(centers(index) - Vec(side / 2.0, side / 2.0), Vec(side, side))
      else canvas.circle(centers(index), side)

object DragShapes extends (SketchContext => DragShapes):
  def apply(context: SketchContext): DragShapes = new DragShapes(context)
