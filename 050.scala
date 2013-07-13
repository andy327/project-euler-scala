/*
The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
*/

def isPrime(n: Long) = (n >= 2 && (2 to scala.math.sqrt(n).toInt).dropWhile(n % _ != 0).size == 0)
val primes = Stream.from(2).filter(isPrime(_))
val primesUnder1M = primes.takeWhile(_ < 1000000).toList
val sizeLimit = Stream.from(1).indexWhere(i => primes.take(i).sum > 1000000)
def sub1MStreaks(windowSize: Int): Iterator[List[Int]] = primesUnder1M.sliding(windowSize, 1).takeWhile(_.sum < 1000000)
val A50 = (sizeLimit to 1 by -1).iterator.flatMap(sub1MStreaks(_)).map(_.sum).find(isPrime(_)).get
