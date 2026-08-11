package genart

class KeyboardSuite extends munit.FunSuite:
  test("keyboard names normalize letters and distinguish named keys") {
    assertEquals(Key.fromName("F"), Some(Key.Character("f")))
    assertEquals(Key.fromName("F12"), Some(Key.Function(12)))
    assertEquals(Key.fromName("ArrowLeft"), Some(Key.ArrowLeft))
    assertEquals(Key.fromName(" "), Some(Key.Space))
    assertEquals(Key.fromName("Dead"), None)
  }

  test("printable keys accept single Unicode code points") {
    assertEquals(Key.fromName("é"), Some(Key.Character("é")))
    assertEquals(
      Key.fromName("\ud83d\ude00"),
      Some(Key.Character("\ud83d\ude00"))
    )
    assertEquals(
      Key.fromName("\ud801\udc00"),
      Some(Key.Character("\ud801\udc28"))
    )
    for invalid <- List("", "ab", "e\u0301", "\n", "\u007f", "\ud801", "\udc00")
    do assertEquals(Key.fromName(invalid), None)
  }
