/*
A unit fraction contains 1 in the numerator.
The decimal representation of the unit fractions with denominators 2 to 10 are given:

1/2   =   0.5
1/3   =   0.(3)
1/4   =   0.25
1/5   =   0.2
1/6   =   0.1(6)
1/7   =   0.(142857)
1/8   =   0.125
1/9   =   0.(1)
1/10  =   0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle.
It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
*/

def coprime10Factor(n: Int): Int = {
  require(n > 0)
  (n % 2, n % 5) match {
    case (0, 0) => coprime10Factor(n / 10)
    case (0, r5) => coprime10Factor(n / 2)
    case (r2, 0) => coprime10Factor(n / 5)
    case (r2, r5) => n
  }
}
def repeatingPeriod(n: Int): Int = {
  val maxFactor = coprime10Factor(n)
  if (maxFactor == 1) 0
  else Stream.iterate(10 % maxFactor)(k => (k * 10) % maxFactor).takeWhile(_ > 1).length + 1
}
val A26 = (1 to 1000).maxBy(repeatingPeriod(_))
