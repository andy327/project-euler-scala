/*
The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3).
In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are cube.
*/

def isCube(n: Long): Boolean = scala.math.cbrt(n) % 1 == 0
val cubes = Stream.iterate(0L)(_ + 1).map(scala.math.pow(_, 3).toLong)
def arePermutations(n1: Long, n2: Long): Boolean = n1.toString.sorted == n2.toString.sorted
def countCubePermutations(n: Long): Int = {
  val nCubed = scala.math.pow(n, 3).toLong
  cubes.takeWhile(_ <= nCubed * 10).filter(arePermutations(_, nCubed)).size
}
val A62 = Stream.iterate(1L)(_ + 1).find(countCubePermutations(_) == 5).map(scala.math.pow(_, 3).toLong).get


val cubes = Stream.iterate(0L)(_ + 1).map(scala.math.pow(_, 3).toLong)
def isCube(n: Long): Boolean = scala.math.cbrt(n) % 1 == 0
def arePermutations(n1: Long, n2: Long): Boolean = n1.toString.sorted == n2.toString.sorted
def countCubePermutations(n: Long): Int = {
  val nCubed = scala.math.pow(n, 3).toLong
  cubes.takeWhile(_ <= nCubed * 10).filter(arePermutations(_, nCubed)).size
}
def lowestCubePermutation(n: Long): Long = {
  val nCubed = scala.math.pow(n, 3).toLong
  cubes.takeWhile(_ <= nCubed * 10).filter(arePermutations(_, nCubed)).min
}
val cubeMap = collection.mutable.Map[String, Int]().withDefaultValue(0)
val highRoot = cubes
  .map(cb => { cubeMap(cb.toString.sorted) += 1; cb.toString.sorted })
  .zipWithIndex
  .find{ case (n, i) => cubeMap(n) == 5 && countCubePermutations(i) == 5 }
  .get._2
val A62 = lowestCubePermutation(highRoot.toLong)
