/*
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
*/

def isPrime(n: Long) = (n >= 2 && (2 to scala.math.sqrt(n).toInt).dropWhile(n % _ != 0).size == 0)
val A10 = Stream.iterate(2L)(_ + 1).filter(isPrime(_)).takeWhile(_ < 2000000).sum
