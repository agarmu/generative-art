package examples

import genart.SketchHandle
import genart.p5.P5Provider
import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.JSConverters.*

@JSExportTopLevel("BrowserExampleRunner")
final class BrowserExampleRunner(container: dom.Element) extends js.Object:
  private var current = Option.empty[SketchHandle]

  def startExample(name: String): Unit =
    Examples.select(name).fold[Unit](
      error =>
        dom.console.error(error + s". Available: ${Examples.names.mkString(", ")}"),
      create =>
        endExample()
        current = Some(P5Provider.run(container)(create))
    )

  def endExample(): Unit =
    val previous = current
    current = None
    previous.foreach(_.stop())

  def isRunning(): Boolean = current.exists(handle => !handle.isStopped)

object BrowserExampleRunner:
  @JSExportTopLevel("exampleNames")
  def names(): js.Array[String] = Examples.names.toJSArray
