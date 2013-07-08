/*
Starting in the top left corner of a 2x2 grid, and only being able to move to the right and down,
there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20x20 grid?
*/

def pathCount(rows: Int, cols: Int): Long = {
  var pathTable = Map.empty[(Int, Int), Long]
  def sumCount(i: Int, j: Int): Long = {
    lazy val count = (i, j) match {
      case (0, j) => 1
      case (i, 0) => 1
      case (i, j) => sumCount(i, j - 1) + sumCount(i - 1, j)
    }
    val result = pathTable.getOrElse((i, j), count)
    pathTable += (i, j) -> result
    result
  }
  sumCount(rows, cols)
}
val A15 = pathCount(20, 20)
