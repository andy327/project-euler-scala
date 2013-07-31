/*
The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3).
In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are cube.
*/

val cubes = Stream.iterate(0L)(_ + 1).map(scala.math.pow(_, 3).toLong)
val cubeMap = collection.mutable.Map[String, Int]().withDefaultValue(0)
def key(n: Long) = n.toString.sorted
def cubePermutations(n: Int): Seq[Long] = {
  val nCubed = scala.math.pow(n, 3).toLong
  cubes.takeWhile(_ <= nCubed * 10).filter(key(_) == key(nCubed)).toSeq
}
val cubeKeys = cubes.map(cb => { cubeMap(key(cb)) += 1; key(cb) })
val highRoot = cubeKeys.zipWithIndex.find{ case (cb, n) => cubeMap(cb) >= 5 && cubePermutations(n).size == 5 }.get._2.toInt
val A62 = cubePermutations(highRoot).min
