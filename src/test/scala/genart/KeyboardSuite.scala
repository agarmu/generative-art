package genart

class KeyboardSuite extends munit.FunSuite:
  test("keyboard names normalize letters and distinguish named keys") {
    assertEquals(Key.fromName("F"), Some(Key.Character("f")))
    assertEquals(Key.fromName("F12"), Some(Key.Function(12)))
    assertEquals(Key.fromName("ArrowLeft"), Some(Key.ArrowLeft))
    assertEquals(Key.fromName(" "), Some(Key.Space))
    assertEquals(Key.fromName("Dead"), None)
  }
