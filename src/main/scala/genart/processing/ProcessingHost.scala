package genart.processing

import genart.{Sketch, SketchContext}
import genart.lib.runtime.{RunConfig, SketchSession}
import processing.core.PApplet
import java.util.concurrent.atomic.AtomicBoolean

private final class ProcessingHost(
    config: RunConfig,
    create: SketchContext => Sketch,
    window: ProcessingProvider.Window
) extends PApplet:
  private var session = Option.empty[SketchSession]
  private var dimensions = (0, 0)
  private var disposing = false
  private val stopRequested = new AtomicBoolean(false)
  private val callbackLock = new Object
  private var animationThread = Option.empty[Thread]
  private var inCallback = false
  private var waking = false

  def requestStop(): Unit =
    stopRequested.set(true)
    wakeOnStop()

  private def wakeOnStop(): Unit = callbackLock.synchronized {
    if stopRequested.get() && !waking then
      animationThread.foreach { target =>
        waking = true
        val wakeup: Runnable = () =>
          while target.isAlive do
            callbackLock.synchronized {
              while inCallback do callbackLock.wait()
              target.interrupt()
            }
            target.join(20L)
        new Thread(wakeup, "genart-stop-wakeup").start()
      }
  }

  private def callback(body: => Unit): Unit =
    callbackLock.synchronized {
      animationThread = Some(Thread.currentThread())
      inCallback = true
      if stopRequested.get() then Thread.interrupted()
    }
    wakeOnStop()
    try body
    finally
      callbackLock.synchronized {
        inCallback = false
        callbackLock.notifyAll()
      }

  override def settings(): Unit =
    window.size match
      case Some((width, height)) => size(width, height)
      case None                  => fullScreen()
    pixelDensity(1)

  override def setup(): Unit = callback {
    getSurface.setTitle(window.title)
    session = Some(
      SketchSession.start(new ProcessingProvider(this), config)(create)
    )
    dimensions = (width, height)
    getSurface.setResizable(window.size.nonEmpty && window.resizable)
  }

  override def draw(): Unit = callback {
    session.foreach { active =>
      if !stopRequested.get() then
        if dimensions != (width, height) then
          active.resize(width, height)
          dimensions = (width, height)
        active.frame(System.nanoTime() * 1e-9)
      if stopRequested.get() then active.stop()
    }
  }

  override def exit(): Unit = requestStop()

  override def dispose(): Unit =
    if !disposing then
      disposing = true
      try session.foreach(_.stop())
      finally
        try super.dispose()
        finally requestStop()

  override def exitActual(): Unit = ()
