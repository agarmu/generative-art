package examples

import genart.processing.ProcessingProvider

object ProcessingExampleRunner:
  def main(args: Array[String]): Unit =
    val availableExamples = Examples.names.mkString(", ")
    val selected = for
      name <- args match
        case Array(name) => Right(name)
        case _           => Left("Usage: ProcessingExampleRunner <example>")
      create <- Examples.select(name)
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
