/*
Pentagonal numbers are generated by the formula, Pn=n(3n−1)/2.
The first ten pentagonal numbers are:

1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...

It can be seen that P4 + P7 = 22 + 70 = 92 = P8.
However, their difference, 70 − 22 = 48, is not pentagonal.

Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference are pentagonal
and D = |Pk − Pj| is minimised; what is the value of D?
*/

def isPentagonal(n: Int): Boolean = (1 + scala.math.sqrt(24 * n + 1)) % 6 == 0
def streamComboIterator[A, B](s1: Stream[A], s2: Stream[B]): Iterator[(A, B)] =
  Stream.from(0).flatMap{ a => (0 to a).map(i => (i, a - i)) }.map{ case (a, b) => (s1(a), s2(b)) }.iterator
val pentagonals = Stream.from(1).map(n => n * (3 * n - 1) / 2)
val A44 = streamComboIterator(pentagonals, pentagonals)
  .filter{ case (a, b) => a < b && isPentagonal(a + b) && isPentagonal(b - a) }
  .map{ case (a, b) => b - a }.next