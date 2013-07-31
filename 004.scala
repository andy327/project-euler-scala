/*
A palindromic number reads the same both ways. The largest palindrome made from the
product of two 2-digit numbers is 9009 = 91 x 99.

Find the largest palindrome made from the product of two 3-digit numbers.
*/

def digits(n: Int) = n.toString.toList.map(_.asDigit)
def isPalindrome(n: Int) = digits(n) == digits(n).reverse
val A4 = (100 to 999).map(a => (999 to 100 by -1).view.map(b => a * b).find(isPalindrome(_)).getOrElse(0)).max
