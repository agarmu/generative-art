package genart

/** Native renderer blend modes. Normal uses source-over on both backends.
  * Artistic modes follow native compositing semantics: Processing combines
  * alpha differently from Canvas2D, especially on transparent surfaces. Use
  * opaque backgrounds when matching artistic blends across backends.
  */
enum BlendMode:
  case Normal, Add, Multiply, Screen, Darken, Lighten, Difference, Exclusion
