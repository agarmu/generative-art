package genart.p5

import genart.{Key, KeyEvent, KeyModifiers}
import org.scalajs.dom
import scala.scalajs.js

private[p5] final class P5KeyboardInput(
    canvas: dom.HTMLCanvasElement,
    emit: KeyEvent => Unit
):
  private var pressed = Map.empty[String, Key]

  private def modifiers(event: dom.KeyboardEvent): KeyModifiers =
    KeyModifiers(event.shiftKey, event.ctrlKey, event.altKey, event.metaKey)

  private def identity(event: dom.KeyboardEvent): String =
    if event.code.nonEmpty then event.code else event.key

  private val down: js.Function1[dom.Event, Unit] = raw =>
    val event = raw.asInstanceOf[dom.KeyboardEvent]
    if !event.isComposing then
      val code = identity(event)
      pressed.get(code).orElse(Key.fromName(event.key)).foreach { key =>
        val repeat = event.repeat || pressed.contains(code)
        pressed = pressed.updated(code, key)
        emit(KeyEvent.Pressed(key, modifiers(event), repeat))
        if key != Key.Tab && !event.ctrlKey && !event.metaKey && !event.altKey
        then event.preventDefault()
      }

  private val up: js.Function1[dom.Event, Unit] = raw =>
    val event = raw.asInstanceOf[dom.KeyboardEvent]
    val code = identity(event)
    pressed.get(code).foreach { key =>
      pressed -= code
      emit(KeyEvent.Released(key, modifiers(event)))
    }

  private val focus: js.Function1[dom.Event, Unit] = _ => canvas.focus()
  private val blur: js.Function1[dom.Event, Unit] = _ => cancel()

  def cancel(): Unit =
    pressed = Map.empty
    emit(KeyEvent.Cancelled)

  def start(): Unit =
    canvas.tabIndex = 0
    canvas.setAttribute("aria-label", "Sketch canvas")
    canvas.addEventListener("pointerdown", focus)
    canvas.addEventListener("keydown", down)
    canvas.addEventListener("keyup", up)
    canvas.addEventListener("blur", blur)
    dom.window.addEventListener("blur", blur)

  def close(): Unit =
    canvas.removeEventListener("pointerdown", focus)
    canvas.removeEventListener("keydown", down)
    canvas.removeEventListener("keyup", up)
    canvas.removeEventListener("blur", blur)
    dom.window.removeEventListener("blur", blur)
    pressed = Map.empty
