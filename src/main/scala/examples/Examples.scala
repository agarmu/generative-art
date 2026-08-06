package examples

import genart.{Sketch, SketchContext}

private[examples] object Examples:
  val all: Map[String, SketchContext => Sketch] = Map(
    "Ellipses" -> Ellipses,
    "Square" -> Square
  )

  val names: Vector[String] = all.keys.toVector.sorted

  def select(name: String): Either[String, SketchContext => Sketch] =
    all.get(name).toRight(s"Unknown example '$name'")
