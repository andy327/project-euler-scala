/*
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once;
for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 x 186 = 7254, containing multiplicand, multiplier,
and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
*/

def isProductPandigital(n: Int): Boolean = {
  val nDigits = n.toString.toIndexedSeq.map(d => d.asDigit)
  val otherDigits = ((1 to 9).toSet -- nDigits).toIndexedSeq
  lazy val hasProduct = (1 to otherDigits.size / 2)
    .map(otherDigits.combinations(_)).flatten
    .map(_.permutations).flatten
    .map(m1 => (otherDigits diff m1).permutations.map((m1, _)).toList).flatten
    .exists{ case (m1, m2) => m1.mkString.toInt * m2.mkString.toInt == n }
  !nDigits.contains(0) && nDigits == nDigits.distinct && hasProduct
}
val A32 = (1234 to 9876).filter(isProductPandigital(_)).sum
