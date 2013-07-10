/*
The number, 197, is called a circular prime because all rotations of the digits:
197, 971, and 719, are themselves prime.

There are thirteen such primes below 100:
2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?
*/

def rotations(s: String): Seq[Int] = (1 to s.size - 1).map(i => List(s.drop(i), s.take(i)).mkString.toInt)
def isPrime(n: Long) = (n >= 2 && (2 to scala.math.sqrt(n).toInt).dropWhile(n % _ != 0).size == 0)
val A35 = (2 until 1000000).filter(isPrime(_)).filter(n => rotations(n.toString).forall(isPrime(_))).size
