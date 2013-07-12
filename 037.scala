/*
The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right,
and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
*/

def isPrime(n: Long) = (n >= 2 && (2 to scala.math.sqrt(n).toInt).dropWhile(n % _ != 0).size == 0)
def slices(s: String): Seq[Int] = (1 to s.size - 1).map(s.take(_).toInt) ++ (1 to s.size - 1).map(s.drop(_).toInt)
val A37 = Stream.from(11).filter(isPrime(_)).filter(p => slices(p.toString).forall(isPrime(_))).take(11).sum
