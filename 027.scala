/*
Euler discovered the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39.
However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41,
and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

The incredible formula  n²  79n + 1601 was discovered, which produces 80 primes for
the consecutive values n = 0 to 79.
The product of the coefficients, 79 and 1601, is 126479.

Considering quadratics of the form:

n² + an + b, where |a| < 1000 and |b| < 1000

where |n| is the modulus/absolute value of n
e.g. |11| = 11 and |4| = 4

Find the product of the coefficients, a and b, for the quadratic expression that
produces the maximum number of primes for consecutive values of n, starting with n = 0.
*/

def isPrime(n: Long) = (n >= 2 && (2 to scala.math.sqrt(n).toInt).dropWhile(n % _ != 0).size == 0)
val primes = Stream.iterate(2)(_ + 1).filter(isPrime(_))
val primesUnder1k = primes.takeWhile(_ < 1000)
def func(a: Int, b: Int): Int => Int = (n: Int) => scala.math.pow(n, 2).toInt + a * n + b
def primeStreak(a: Int, b: Int): Int = {
  val f = func(a, b)
  Stream.iterate(0)(_ + 1).indexWhere(n => !isPrime(f(n)))
}
val testPairs = for (b <- primesUnder1k; a <- (-999 to 999)) yield (a, b)
val topPair = testPairs.maxBy(ab => { val (a, b) = ab; primeStreak(a, b) })
val A27 = topPair._1 * topPair._2
