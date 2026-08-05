package genart.p5

import genart.{Sketch, SketchContext, SketchHandle}
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
  private var starting = false
  private var stopRequested = false
  private var closed = false

  private val visibilityChanged: js.Function1[dom.Event, Unit] = _ =>
    session.foreach { active =>
      if dom.document.hidden then active.suspend()
      else active.resume()
    }

  def start(): Unit =
    val bind: js.Function1[P5, Unit] = native =>
      instance = Some(native)
      native.setup = () => setup(native)
      native.draw = () =>
        if !stopRequested then
          session.foreach(_.frame(dom.window.performance.now() / 1000.0))
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
          active.resize(width, height)
      }

  private def release(): Unit =
    if !closed then
      closed = true
      stopRequested = true
      observer.foreach(_.disconnect())
      dom.document.removeEventListener("visibilitychange", visibilityChanged)
      instance.foreach { native =>
        native.noLoop()
        native.remove()
      }
