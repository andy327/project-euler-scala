/*
The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order
the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes,
792, represents the lowest sum for a set of four primes with this property.

Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.
*/

def isPrime(n: Long) = (n >= 2 && (2 to scala.math.sqrt(n).toInt).dropWhile(n % _ != 0).size == 0)
val primesUnder10k = Stream.from(3).filter(isPrime(_)).filterNot(_ == 5).takeWhile(_ < 10000)
def isPrimeSetValid(primes: Seq[Int]): Boolean = {
  val pairs = primes.combinations(2).flatMap(_.permutations)
  pairs.map(pair => pair.map(_.toString).mkString.toInt).forall(isPrime(_))
}
def fiveSetsWithN(n: Int): List[Set[Int]] = {
  val pairsWithN = primesUnder10k.filter(p => isPrimeSetValid(Seq(n, p))).toList
  val pairsFromN = pairsWithN.combinations(2).filter(isPrimeSetValid(_)).toList
  val fourSetsFromN = pairsFromN.combinations(2).map(_.flatten.distinct).filter(_.size == 4).filter(isPrimeSetValid(_))
  fourSetsFromN.map(_.toSet + n).toList.distinct
}
val fiveSets = primesUnder10k.map(fiveSetsWithN(_)).filter(!_.isEmpty).map(_.flatten)
val A60 = fiveSets.head.sum
