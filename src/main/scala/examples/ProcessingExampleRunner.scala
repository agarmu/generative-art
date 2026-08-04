package examples

import genart.{Sketch, SketchContext}
import genart.processing.ProcessingProvider

object ProcessingExampleRunner:
  def main(args: Array[String]): Unit =
    val examples: Map[String, SketchContext => Sketch] = Map(
      "Square" -> Square
    )
    val availableExamples = examples.keys.toVector.sorted.mkString(", ")
    val selected = for
      name <- args match
        case Array(name) => Right(name)
        case _           => Left("Usage: ProcessingExampleRunner <example>")
      create <- examples
        .get(name)
        .toRight(
          s"Unknown example '$name'"
        )
    yield (name, create)

    selected.fold[Unit](
      error => Console.err.println(error + s". Available: ${availableExamples}"),
      (name, create) =>
        ProcessingProvider.run(
          window = ProcessingProvider.Window(
            title = name,
            size = Some((960, 720))
          )
        )(create)
    )
