/*
Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:

Triangle    Tn=n(n+1)/2   1, 3, 6, 10, 15, ...
Pentagonal  Pn=n(3n−1)/2  1, 5, 12, 22, 35, ...
Hexagonal   Hn=n(2n−1)    1, 6, 15, 28, 45, ...

It can be verified that T285 = P165 = H143 = 40755.

Find the next triangle number that is also pentagonal and hexagonal.
*/

def isPentagonal(n: Long): Boolean = (1 + scala.math.sqrt(24 * n + 1)) % 6 == 0
def isHexagonal(n: Long): Boolean = (1 + scala.math.sqrt(1 + 8 * n)) % 4 == 0
val hexagons = Stream.from(144).map(n => n * (2 * n - 1))
val A45 = hexagons.filter(isPentagonal(_)).head