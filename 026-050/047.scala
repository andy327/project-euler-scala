/*
The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2^2 × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors.
What is the first of these numbers?
*/

def primeFactors(n: Long): List[Long] = n match {
  case 1 => List.empty[Long]
  case n => {
    val next = (2 to scala.math.sqrt(n).toInt).dropWhile(n % _ != 0)
    if (next.size == 0) List(n)
    else next.head :: primeFactors(n / next.head)
  }
}
def primeFactorCount(n: Long): Int = primeFactors(n).toSet.size
val A47 = Stream.from(1).filter(primeFactorCount(_) == 4).sliding(4, 1).find(l => l.last == l.head + 3).get.head
