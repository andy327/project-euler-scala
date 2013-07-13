/*
It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.

9 = 7 + 2×1^2
15 = 7 + 2×2^2
21 = 3 + 2×3^2
25 = 7 + 2×3^2
27 = 19 + 2×2^2
33 = 31 + 2×1^2

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
*/

def isPrime(n: Long) = (n >= 2 && (2 to scala.math.sqrt(n).toInt).dropWhile(n % _ != 0).size == 0)
def isSpecial(n: Long): Boolean = (1 to scala.math.sqrt(n / 2).toInt).exists(a => isPrime(n - 2 * a * a))
val A46 = Stream.iterate(3L)(_ + 2).filterNot(isPrime(_)).find(!isSpecial(_)).get
