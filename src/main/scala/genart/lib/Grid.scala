package genart.lib

import scala.reflect.ClassTag

/** Mutable rectangular storage, addressed by column and row. */
final class Grid[A] private (
    val cols: Int,
    val rows: Int,
    private val cells: Array[A]
):
  def contains(col: Int, row: Int): Boolean =
    col >= 0 && col < cols && row >= 0 && row < rows

  def apply(col: Int, row: Int): A = cells(index(col, row))

  def update(col: Int, row: Int, value: A): Unit =
    cells(index(col, row)) = value

  /** Evaluate the value separately for each cell. */
  def fill(value: => A): Unit =
    cells.indices.foreach(index => cells(index) = value)

  private def index(col: Int, row: Int): Int =
    if !contains(col, row) then
      throw new IndexOutOfBoundsException(
        s"Cell ($col, $row) outside ${cols}x$rows grid"
      )
    row * cols + col

object Grid:
  def fill[A: ClassTag](cols: Int, rows: Int)(value: => A): Grid[A] =
    require(cols >= 0 && rows >= 0, "grid dimensions must be nonnegative")
    require(
      cols.toLong * rows <= Int.MaxValue,
      "grid cell count must fit in an Int"
    )
    new Grid(cols, rows, Array.fill(cols * rows)(value))
