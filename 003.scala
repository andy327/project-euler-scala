/*
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
*/

def primeFactors(n: Long): List[Long] = n match {
  case 1 => List.empty[Long]
  case n => {
    val next = (2 to scala.math.sqrt(n).toInt).dropWhile(n % _ != 0)
    if (next.size == 0) List(n)
    else next.head :: primeFactors(n / next.head)
  }
}
val A3 = primeFactors(600851475143L).max
