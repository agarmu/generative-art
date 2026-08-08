package examples

import genart.{Sketch, SketchContext}

private[examples] object Examples:
  final case class Example(
      name: String,
      ident: String,
      factory: SketchContext => Sketch
  )

  val all: Vector[Example] = Vector(
    Example("Basic Shapes", "basic", BasicShapes),
    Example("Color Walk", "walk", ColorWalk),
    Example("Drag Shapes", "drag", DragShapes),
    Example("Paths", "paths", Paths),
    Example("Strokes", "strokes", Strokes),
    Example("Polygons", "polygons", Polygons)
  ).sortBy(_.name)

  val idents: Vector[String] = all.map(_.ident)

  def select(ident: String): Either[String, Example] =
    all.find(_.ident == ident).toRight(s"Unknown example '$ident'")
