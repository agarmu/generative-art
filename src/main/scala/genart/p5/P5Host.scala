package genart.p5

import genart.{KeyEvent, PointerEvent, Sketch, SketchContext, SketchHandle}
import genart.lib.runtime.{RunConfig, SketchSession}
import org.scalajs.dom
import scala.scalajs.js

private final class P5Host(
    container: dom.Element,
    config: RunConfig,
    create: SketchContext => Sketch
) extends SketchHandle:
  private var instance = Option.empty[P5]
  private var session = Option.empty[SketchSession]
  private var observer = Option.empty[dom.ResizeObserver]
  private var input = Option.empty[P5PointerInput]
  private var keyboard = Option.empty[P5KeyboardInput]
  private var inputEvents = Vector.empty[PointerEvent | KeyEvent]
  private var starting = false
  private var stopRequested = false
  private var closed = false

  private val visibilityChanged: js.Function1[dom.Event, Unit] = _ =>
    session.foreach { active =>
      if dom.document.hidden then
        input.foreach(_.cancel())
        keyboard.foreach(_.cancel())
        inputEvents = Vector(PointerEvent.Cancelled, KeyEvent.Cancelled)
        active.suspend()
      else active.resume()
    }

  def start(): Unit =
    val bind: js.Function1[P5, Unit] = native =>
      instance = Some(native)
      native.setup = () => setup(native)
      native.draw = () =>
        if !stopRequested then
          session.foreach { active =>
            if !active.isSuspended then
              val events = inputEvents
              inputEvents = Vector.empty
              events.foreach {
                case event: PointerEvent => active.pointer(event)
                case event: KeyEvent     => active.key(event)
              }
              active.frame(dom.window.performance.now() / 1000.0)
          }
    new P5(bind, container)
    ()

  def stop(): Unit =
    stopRequested = true
    session.foreach(_.stop())
    if session.isEmpty && !starting then release()

  def isStopped: Boolean = stopRequested

  private def setup(native: P5): Unit =
    if stopRequested then
      native.noLoop()
      native.remove()
    else
      starting = true
      try
        val provider = new P5Provider(native, container, () => release())
        val active = SketchSession.start(provider, config)(create)
        session = Some(active)
        if stopRequested then active.stop()
        else
          val pointerInput = new P5PointerInput(
            native,
            event => if !stopRequested then inputEvents :+= event
          )
          input = Some(pointerInput)
          pointerInput.start()
          val keyboardInput = new P5KeyboardInput(
            native.canvas,
            event => if !stopRequested then inputEvents :+= event
          )
          keyboard = Some(keyboardInput)
          keyboardInput.start()
          val resizeObserver = new dom.ResizeObserver((_, _) => resize())
          observer = Some(resizeObserver)
          resizeObserver.observe(container)
          dom.document.addEventListener("visibilitychange", visibilityChanged)
          if dom.document.hidden then active.suspend()
      finally starting = false

  private def resize(): Unit =
    if !stopRequested then
      session.foreach { active =>
        val (width, height) = P5Provider.dimensions(container)
        val dimensions = instance.map(native => (native.width, native.height))
        if width > 0 && height > 0 && !dimensions.contains((width, height)) then
          input.foreach(_.cancel())
          inputEvents = inputEvents.collect { case event: KeyEvent =>
            event
          } :+ PointerEvent.Cancelled
          active.resize(width, height)
      }

  private def release(): Unit =
    if !closed then
      closed = true
      stopRequested = true
      inputEvents = Vector.empty
      input.foreach(_.close())
      keyboard.foreach(_.close())
      observer.foreach(_.disconnect())
      dom.document.removeEventListener("visibilitychange", visibilityChanged)
      instance.foreach { native =>
        native.noLoop()
        native.remove()
      }
