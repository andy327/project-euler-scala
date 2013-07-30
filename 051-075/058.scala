/*
Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.

37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is
that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ~ 62%.

If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed.
If this process is continued, what is the side length of the square spiral for which the ratio of primes along
both diagonals first falls below 10%?
*/

def isPrime(n: Long) = (n >= 2 && (2 to scala.math.sqrt(n).toInt).dropWhile(n % _ != 0).size == 0)
def cornerValues(layer: Int): Seq[Int] = {
  require(layer > 0)
  val side = 2 * layer + 1
  val max = side * side
  (0 to 3).map(max - _ * (side - 1))
}
val primeCounts = Stream.iterate((0, 0)) { case (i, n) => (i + 1, n + cornerValues(i + 1).filter(isPrime(_)).size) }
val ratios = primeCounts.map{ case (i, c) => c.toDouble / (4 * i + 1) }
val A58 = 3 + 2 * ratios.tail.indexWhere(_ < .1)
