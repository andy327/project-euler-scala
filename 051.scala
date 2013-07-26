/*
By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values:
13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example
having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits)
with the same digit, is part of an eight prime value family.
*/

def isPrime(n: Long) = (n >= 2 && (2 to scala.math.sqrt(n).toInt).dropWhile(n % _ != 0).size == 0)
val primes5to9Digits = Stream.range(10000, 1000000).filter(isPrime(_))
def masks(n: Int): IndexedSeq[String] = (0 to 9)
  .map(i => n.toString.replaceAll(i.toString, "*"))
  .filter(_.contains("*"))
val maxMasks = primes5to9Digits
  .map(masks(_))
  .map(ms => ms
    .map(m => (0 to 9)
      .map(i => m.replaceAllLiterally("*", i.toString))
      .filter(x => isPrime(x.toLong))
      .filterNot(_.startsWith("0")))
    .maxBy(_.size))
val A51 = maxMasks.find(_.size == 8).get.head
