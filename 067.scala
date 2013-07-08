/*
By starting at the top of the triangle below and moving to adjacent numbers on the row below,
the maximum total from top to bottom is 23.

   3
  7 4
 2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom in triangle.txt, a 15K text file containing a triangle with one-hundred rows.

NOTE: This is a much more difficult version of Problem 18.
It is not possible to try every route to solve this problem, as there are 299 altogether!
If you could check one trillion (1012) routes every second it would take over twenty billion years to check them all.
There is an efficient algorithm to solve it. ;o)
*/

val triangleBig = scala.io.Source.fromFile("triangle.txt").mkString.split("\n").toList
def bestPathSum(triangleStrings: List[String]): Int = {
  val triangle = triangleStrings.map(_.sliding(2, 3).map(_.toInt).toList)
  val indexedTriangle = triangle.zipWithIndex.map(x => x._1.map((_, x._2)).zipWithIndex).map(_.map(x => (x._1._1, x._1._2, x._2))).flatten
  val triangleMap = indexedTriangle.map(x => (x._2, x._3) -> x._1).toMap
  val pathSize = triangle.size
  var pathTable = Map.empty[(Int, Int), Int]
  def maxPathSum(i: Int, j: Int): Int = {
    lazy val sum = (i, j) match {
      case (i, j) if (i == pathSize - 1) => triangleMap.get((i, j)).get
      case (i, j) => triangleMap.get((i, j)).get + scala.math.max(maxPathSum(i + 1, j), maxPathSum(i + 1, j + 1))
    }
    val result = pathTable.getOrElse((i, j), sum)
    pathTable += (i, j) -> result
    result
  }
  maxPathSum(0, 0)
}
val A67 = bestPathSum(triangleBig)
