/*
The decimal number, 585 = 1001001001_2 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)
*/

def toBinaryString(n: Int): String = Stream.iterate(n)(_ / 2).takeWhile(_ > 0).map(_ % 2).reverse.mkString
def isBase2Palindromic(n: Int): Boolean = { val bN = toBinaryString(n); bN == bN.reverse }
def isBase10Palindromic(n: Int): Boolean = n == n.toString.reverse.toInt
val A36 = (1 until 1000000).filter(n => isBase10Palindromic(n) && isBase2Palindromic(n)).sum
