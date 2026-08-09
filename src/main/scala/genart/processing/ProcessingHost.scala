package genart.processing

import genart.{KeyEvent, PointerButton, PointerEvent, Sketch, SketchContext}
import genart.lib.math.Vec
import genart.lib.runtime.{RunConfig, SketchSession}
import processing.core.{PApplet, PConstants}
import processing.event.{KeyEvent as NativeKey, MouseEvent}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

private final class ProcessingHost(
    config: RunConfig,
    create: SketchContext => Sketch,
    window: ProcessingProvider.Window
) extends PApplet:
  private var session = Option.empty[SketchSession]
  private var dimensions = (0, 0)
  private var disposing = false
  private val stopRequested = new AtomicBoolean(false)
  private val inputEvents = new AtomicReference(
    Vector.empty[PointerEvent | KeyEvent]
  )
  private val keyboard = new ProcessingKeyboardInput(enqueue)
  private val callbackLock = new Object
  private var animationThread = Option.empty[Thread]
  private var inCallback = false
  private var waking = false

  def requestStop(): Unit =
    stopRequested.set(true)
    wakeOnStop()

  def isStopped: Boolean = stopRequested.get()

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
    keyRepeatEnabled = true
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
          inputEvents.updateAndGet(events =>
            events.collect { case event: KeyEvent =>
              event
            } :+ PointerEvent.Cancelled
          )
          active.resize(width, height)
          dimensions = (width, height)
        inputEvents.getAndSet(Vector.empty).foreach {
          case event: PointerEvent => active.pointer(event)
          case event: KeyEvent     => active.key(event)
        }
        active.frame(System.nanoTime() * 1e-9)
      if stopRequested.get() then active.stop()
    }
  }

  override def exit(): Unit = requestStop()

  private def enqueue(event: PointerEvent | KeyEvent): Unit =
    if !stopRequested.get() then
      inputEvents.updateAndGet(_ :+ event)
      ()

  private def button(event: MouseEvent): Option[PointerButton] =
    event.getButton match
      case PConstants.LEFT   => Some(PointerButton.Primary)
      case PConstants.CENTER => Some(PointerButton.Middle)
      case PConstants.RIGHT  => Some(PointerButton.Secondary)
      case _                 => None

  override def mouseMoved(event: MouseEvent): Unit =
    enqueue(PointerEvent.Moved(Vec(event.getX.toDouble, event.getY.toDouble)))

  override def mouseDragged(event: MouseEvent): Unit = mouseMoved(event)

  override def mousePressed(event: MouseEvent): Unit =
    button(event).foreach { pressed =>
      enqueue(
        PointerEvent.Pressed(
          Vec(event.getX.toDouble, event.getY.toDouble),
          pressed
        )
      )
    }

  override def mouseReleased(event: MouseEvent): Unit =
    button(event).foreach { released =>
      enqueue(
        PointerEvent.Released(
          Vec(event.getX.toDouble, event.getY.toDouble),
          released
        )
      )
    }

  override def mouseExited(event: MouseEvent): Unit =
    val held = event.getNative match
      case native: java.awt.event.MouseEvent =>
        val buttons = java.awt.event.InputEvent.BUTTON1_DOWN_MASK |
          java.awt.event.InputEvent.BUTTON2_DOWN_MASK |
          java.awt.event.InputEvent.BUTTON3_DOWN_MASK
        (native.getModifiersEx & buttons) != 0
      case _ => mousePressed
    if !held then enqueue(PointerEvent.Cancelled)

  override def keyPressed(event: NativeKey): Unit =
    keyboard.down(event)
    if event.getKey == PConstants.ESC then key = 0.toChar

  override def keyReleased(event: NativeKey): Unit = keyboard.up(event)

  override def focusLost(): Unit =
    super.focusLost()
    enqueue(PointerEvent.Cancelled)
    keyboard.cancel()

  override def dispose(): Unit =
    if !disposing then
      disposing = true
      inputEvents.set(Vector.empty)
      try session.foreach(_.stop())
      finally
        try super.dispose()
        finally requestStop()

  override def exitActual(): Unit = ()
