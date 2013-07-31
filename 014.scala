/*
The following iterative sequence is defined for the set of positive integers:

n -> n/2 (n is even)
n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13  40  20  10  5  16  8  4  2  1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
Although it has not been proved yet (Collatz Problem), it is thought that all starting
numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
*/

def collatzIters(n: Long, acc: Int): (Long, Int) =
  if (n == 1) (n, acc)
  else {
    val next = n match {
      case r if (r % 2 == 0) => n / 2
      case _ => 3 * n + 1
    }
    collatzIters(next, acc + 1)
  }
val A14 = (1L until 1000000L).foldLeft((0L, 0)) {
  (acc: (Long, Int), n: Long) =>
    val next = collatzIters(n, 1)
    if (acc._2 > next._2) acc else (n, next._2)
}._1
