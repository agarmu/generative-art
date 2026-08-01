package genart

import genart.lib.{Grid, requireFinite, requireUnit}
import scala.collection.mutable.ArrayBuffer

class LibrarySuite extends munit.FunSuite:
  test("grid reads and writes preserve cells and reject out-of-bounds access") {
    val grid = Grid.fill(3, 2)(0)
    grid(2, 0) = 7
    grid(0, 1) = 9
    assert(grid(2, 0) == 7)
    assert(grid(0, 1) == 9)
    assert(grid(1, 1) == 0)
    assert(grid.contains(2, 1))
    for (col, row) <- List((-1, 0), (0, -1), (3, 0), (0, 2)) do
      assert(!grid.contains(col, row))
      intercept[IndexOutOfBoundsException](grid(col, row))
      intercept[IndexOutOfBoundsException](grid(col, row) = 1)
    grid.fill(5)
    for col <- 0 until grid.cols; row <- 0 until grid.rows do
      assert(grid(col, row) == 5)

  }

  test("grid initializers create independent mutable values for each cell") {
    val buffers = Grid.fill(2, 1)(ArrayBuffer.empty[Int])
    buffers(0, 0) += 1
    assert(buffers(1, 0).isEmpty)
    buffers.fill(ArrayBuffer.empty[Int])
    buffers(0, 0) += 2
    assert(buffers(1, 0).isEmpty)

  }

  test(
    "empty grids do not evaluate initializers and invalid sizes are rejected"
  ) {
    var evaluations = 0
    val empty = Grid.fill(0, 2) { evaluations += 1; 0 }
    empty.fill { evaluations += 1; 0 }
    assert(evaluations == 0)
    assert(!empty.contains(0, 0))
    for (cols, rows) <- List((-1, 2), (2, -1), (Int.MaxValue, 2)) do
      assertRejected(Grid.fill(cols, rows)(0))

  }

  test("numeric validation rejects non-finite and out-of-range values") {
    for value <- List(-Double.MaxValue, 0.0, Double.MaxValue) do
      requireFinite(value)
    for value <- List(0.0, 0.5, 1.0) do requireUnit(value)
    for value <- List(
        Double.NaN,
        Double.NegativeInfinity,
        Double.PositiveInfinity
      )
    do
      assertRejected(requireFinite(value))
      assertRejected(requireUnit(value))
    assertRejected(requireUnit(-0.1))
    assertRejected(requireUnit(1.1))
  }

  private def assertRejected(action: => Any): Unit =
    intercept[IllegalArgumentException](action)
