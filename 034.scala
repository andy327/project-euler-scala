/*
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
*/

implicit def impFactorial(n: Int) = new { def `!`: BigInt = (BigInt(1) /: (1 to n))(_ * _) }
def isCurious(n: Int): Boolean = n == n.toString.map(d => (d.asDigit)!).sum
val A34 = (10 until 3000000).filter(isCurious(_)).sum
