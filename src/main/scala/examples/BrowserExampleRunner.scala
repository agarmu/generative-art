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

  def startExample(ident: String): Unit =
    Examples.select(ident).fold[Unit](
      error =>
        dom.console.error(error + s". Available: ${Examples.idents.mkString(", ")}"),
      example =>
        endExample()
        current = Some(P5Provider.run(container)(example.factory))
    )

  def endExample(): Unit =
    val previous = current
    current = None
    previous.foreach(_.stop())

  def isRunning(): Boolean = current.exists(handle => !handle.isStopped)

object BrowserExampleRunner:
  @JSExportTopLevel("listExamples")
  def listExamples(): js.Array[js.Dictionary[String]] =
    Examples.all.map { example =>
      js.Dictionary("name" -> example.name, "ident" -> example.ident)
    }.toJSArray
