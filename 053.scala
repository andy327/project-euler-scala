/*
There are exactly ten ways of selecting three from five, 12345:

123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

In combinatorics, we use the notation, 5C3 = 10.

In general,

nCr = n! / r!(n−r)!, where r <= n, n! = n * (n−1) * ... * 3 * 2 * 1, and 0! = 1.

It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.

How many, not necessarily distinct, values of  nCr, for 1 <= n <= 100, are greater than one-million?
*/

def factorial(n: Int): BigInt = (BigInt(1) /: (1 to n))(_ * _)
def choose(n: Int)(r: Int): Long = (1 to r).map(i => (n - (r - i)) / i.toDouble).reduce(_ * _).toLong
def firstRValueOver1M(n: Int): Option[Int] = (1 to n / 2).find(choose(n)(_) > 1000000)
def numRValuesOver1M(n: Int): Int = {
  val r = firstRValueOver1M(n).getOrElse(n)
  (r to n - r).size
}
val A53 = (1 to 100).map(numRValuesOver1M(_)).sum
