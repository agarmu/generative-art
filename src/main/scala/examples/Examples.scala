package examples

import genart.{Sketch, SketchContext}

private[examples] object Examples:
  final case class Example(
      name: String,
      ident: String,
      factory: SketchContext => Sketch
  )

  val all: Vector[Example] = Vector(
    Example("Basic Shapes", "BasicShapes", BasicShapes),
    Example("Color Walk", "ColorWalk", ColorWalk),
    Example("Paths", "Paths", Paths),
    Example("Strokes", "Strokes", Strokes),
    Example("Polygons", "Polygons", Polygons)
  ).sortBy(_.name)

  val idents: Vector[String] = all.map(_.ident)

  def select(ident: String): Either[String, Example] =
    all.find(_.ident == ident).toRight(s"Unknown example '$ident'")
