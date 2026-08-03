package genart.processing

import genart.{Color, Frame, Sketch}
import genart.lib.random.Seed
import genart.lib.runtime.{RunConfig, SketchSession}
import processing.core.PApplet
import java.awt.Component
import java.awt.event.{WindowAdapter, WindowEvent}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.swing.SwingUtilities
import scala.util.control.NonFatal

object ProcessingWindowSmoke:
  def main(args: Array[String]): Unit =
    val firstStopped = new AtomicBoolean(false)
    val completed = new CountDownLatch(2)
    val windowsClosed = new CountDownLatch(2)
    val failure = new AtomicReference[Throwable]()
    val previousHandler = Thread.getDefaultUncaughtExceptionHandler
    Thread.setDefaultUncaughtExceptionHandler((_, error) => {
      failure.compareAndSet(null, error)
      ()
    })
    val probes = Vector(false, true).map { waitForFirst =>
      val applet = new PApplet:
        override def settings(): Unit =
          size(200, 160)
          pixelDensity(1)
        override def draw(): Unit = ()
      val probe =
        new Probe(
          applet,
          waitForFirst,
          firstStopped,
          completed,
          windowsClosed,
          failure
        )
      applet.registerMethod("pre", probe)
      applet.registerMethod("post", probe)
      PApplet.runSketch(Array("genart provider check"), applet)
      probe
    }
    try
      assert(
        completed.await(20, TimeUnit.SECONDS),
        "native windows did not complete"
      )
      assert(
        windowsClosed.await(20, TimeUnit.SECONDS),
        "native windows were not disposed"
      )
      Option(failure.get()).foreach(throw _)
    finally
      probes.foreach(_.stop())
      Thread.setDefaultUncaughtExceptionHandler(previousHandler)

  final class Probe(
      applet: PApplet,
      waitForFirst: Boolean,
      firstStopped: AtomicBoolean,
      completed: CountDownLatch,
      windowsClosed: CountDownLatch,
      failure: AtomicReference[Throwable]
  ):
    private val provider = new ProcessingProvider(applet)
    private var session = Option.empty[SketchSession]
    private var frames = 0
    private var framesAfterFirstStopped = 0
    private var stopped = false

    def pre(): Unit = checked {
      if session.isEmpty then
        val component = applet.getSurface.getNative.asInstanceOf[Component]
        SwingUtilities
          .getWindowAncestor(component)
          .addWindowListener(
            new WindowAdapter:
              override def windowClosed(event: WindowEvent): Unit =
                windowsClosed.countDown()
          )
        session = Some(
          SketchSession.start(provider, RunConfig(Seed.fromLong(1L))) {
            context =>
              new Sketch:
                override def setup(): Unit =
                  context.canvas.background(Color.rgb(0.0, 0.0, 0.0))
                  context.canvas.noStroke()
                  context.canvas.fill(Color.rgb(1.0, 0.0, 0.0))

                def draw(frame: Frame): Unit =
                  context.canvas.rect(0.0, 0.0, 20.0, 20.0)
          }
        )
      if frames == 1 then session.foreach(_.resize(240, 180))
      session.foreach(_.frame(System.nanoTime() * 1e-9))
      assert(provider.canvas.snapshot()(8, 8) == Color.rgb(1.0, 0.0, 0.0))
      if firstStopped.get() then framesAfterFirstStopped += 1
      frames += 1
    }

    def post(): Unit = checked {
      if frames >= 3 && (!waitForFirst || framesAfterFirstStopped >= 2) then
        assert(provider.canvas.width == 240 && provider.canvas.height == 180)
        stop()
        if !waitForFirst then firstStopped.set(true)
    }

    def stop(): Unit =
      if !stopped then
        stopped = true
        try session.fold(provider.close())(_.stop())
        finally completed.countDown()

    private def checked(body: => Unit): Unit =
      try body
      catch
        case NonFatal(error) =>
          failure.compareAndSet(null, error)
          stop()
