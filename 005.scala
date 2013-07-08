/*
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*/

def gcd(a: Long, b: Long): Long = b match {
  case 0 => a
  case _ => gcd(b, a % b)
}
def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)
val A5 = (1L to 20L).reduce(lcm(_, _))
