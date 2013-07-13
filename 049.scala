/*
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways:
(i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property,
but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?
*/

def isPrime(n: Long) = (n >= 2 && (2 to scala.math.sqrt(n).toInt).dropWhile(n % _ != 0).size == 0)
val primePermutations = (1000 until 3400).map(n => n.toString.permutations.filter(_.head != '0').map(_.toInt).filter(p => isPrime(p)).toSet)
val candidates = primePermutations.filter(_.size >= 3).flatMap(set => set.filter(n => set.contains(n + 3330) && set.contains(n + 6660))).toSet
val A49 = (candidates - 1487).map(n => n.toString + (n + 3330).toString + (n + 6660).toString).head
