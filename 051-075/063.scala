/*
The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number, 134217728=8^9, is a ninth power.

How many n-digit positive integers exist which are also an nth power?
*/

def nDigitPowers(n: Int): Seq[BigInt] = Stream.iterate(BigInt(1))(_ + 1)
  .map(_.pow(n))
  .takeWhile(_ < BigInt(10).pow(n))
  .dropWhile(_ < BigInt(10).pow(n - 1))
val upperLimit = Stream.from(1).takeWhile(n => BigInt(9).pow(n).toString.size == n).last
val A63 = (1 to upperLimit).map(nDigitPowers(_).size).sum
