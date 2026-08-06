package examples

import genart.processing.ProcessingProvider

object ProcessingExampleRunner:
  def main(args: Array[String]): Unit =
    val availableExamples = Examples.idents.mkString(", ")
    val selected = for
      ident <- args match
        case Array(ident) => Right(ident)
        case _           => Left("Usage: ProcessingExampleRunner <example>")
      example <- Examples.select(ident)
    yield example

    selected.fold[Unit](
      error => Console.err.println(error + s". Available: ${availableExamples}"),
      example =>
        ProcessingProvider.run(
          window = ProcessingProvider.Window(
            title = example.name,
            size = Some((960, 720))
          )
        )(example.factory)
    )
