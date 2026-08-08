package genart.p5

import genart.{PointerButton, PointerEvent}
import genart.lib.math.Vec
import org.scalajs.dom
import scala.scalajs.js

private[p5] final class P5PointerInput(native: P5, emit: PointerEvent => Unit):
  private var active = Option.empty[Double]
  private var pressed = Set.empty[PointerButton]

  private def position(event: dom.PointerEvent): Option[Vec[2]] =
    val canvas = native.canvas
    val bounds = canvas.getBoundingClientRect()
    if canvas.offsetWidth == 0 || canvas.offsetHeight == 0 then None
    else
      val style = dom.window.getComputedStyle(canvas)
      def pixels(value: String): Double = value.stripSuffix("px").toDouble
      val scaleX = bounds.width / canvas.offsetWidth
      val scaleY = bounds.height / canvas.offsetHeight
      val left = pixels(style.paddingLeft)
      val top = pixels(style.paddingTop)
      val width =
        (canvas.clientWidth - left - pixels(style.paddingRight)) * scaleX
      val height =
        (canvas.clientHeight - top - pixels(style.paddingBottom)) * scaleY
      val originX =
        bounds.left + (pixels(style.borderLeftWidth) + left) * scaleX
      val originY = bounds.top + (pixels(style.borderTopWidth) + top) * scaleY
      Option.when(width > 0.0 && height > 0.0)(
        Vec(
          (event.clientX - originX) * native.width / width,
          (event.clientY - originY) * native.height / height
        )
      )

  private def updateButtons(event: dom.PointerEvent, at: Vec[2]): Unit =
    val buttons = Vector(
      PointerButton.Primary -> 1,
      PointerButton.Middle -> 4,
      PointerButton.Secondary -> 2
    ).collect {
      case (button, bit) if (event.buttons & bit) != 0 => button
    }.toSet
    PointerButton.values.foreach { button =>
      if pressed.contains(button) && !buttons.contains(button) then
        emit(PointerEvent.Released(at, button))
      if buttons.contains(button) && !pressed.contains(button) then
        emit(PointerEvent.Pressed(at, button))
    }
    pressed = buttons

  private def releaseCapture(): Unit =
    val previous = active
    active = None
    previous.foreach { id =>
      if native.canvas.hasPointerCapture(id) then
        native.canvas.releasePointerCapture(id)
    }

  def cancel(): Unit =
    releaseCapture()
    pressed = Set.empty
    emit(PointerEvent.Cancelled)

  private val down: js.Function1[dom.Event, Unit] = raw =>
    val event = raw.asInstanceOf[dom.PointerEvent]
    if event.isPrimary && active.isEmpty && (event.buttons & 7) != 0 then
      position(event).foreach { at =>
        active = Some(event.pointerId)
        native.canvas.setPointerCapture(event.pointerId)
        updateButtons(event, at)
        event.preventDefault()
      }

  private val move: js.Function1[dom.Event, Unit] = raw =>
    val event = raw.asInstanceOf[dom.PointerEvent]
    if event.isPrimary && active.forall(_ == event.pointerId) then
      position(event).foreach { at =>
        if active.nonEmpty then
          updateButtons(event, at)
          if pressed.isEmpty then releaseCapture()
          event.preventDefault()
        emit(PointerEvent.Moved(at))
      }

  private val up: js.Function1[dom.Event, Unit] = raw =>
    val event = raw.asInstanceOf[dom.PointerEvent]
    if active.contains(event.pointerId) then
      position(event).foreach(at => updateButtons(event, at))
      if pressed.isEmpty then releaseCapture()

  private val cancelled: js.Function1[dom.Event, Unit] = raw =>
    if active.contains(raw.asInstanceOf[dom.PointerEvent].pointerId) then
      cancel()

  private val leave: js.Function1[dom.Event, Unit] = _ =>
    if active.isEmpty then emit(PointerEvent.Cancelled)

  private val blur: js.Function1[dom.Event, Unit] = _ => cancel()
  private val contextMenu: js.Function1[dom.Event, Unit] = _.preventDefault()
  private val listeners = Vector(
    "pointerdown" -> down,
    "pointermove" -> move,
    "pointerup" -> up,
    "pointercancel" -> cancelled,
    "lostpointercapture" -> cancelled,
    "pointerleave" -> leave,
    "contextmenu" -> contextMenu
  )

  def start(): Unit =
    native.canvas.style.setProperty("touch-action", "none")
    listeners.foreach { case (name, listener) =>
      native.canvas.addEventListener(name, listener)
    }
    dom.window.addEventListener("blur", blur)

  def close(): Unit =
    listeners.foreach { case (name, listener) =>
      native.canvas.removeEventListener(name, listener)
    }
    dom.window.removeEventListener("blur", blur)
    releaseCapture()
    pressed = Set.empty
