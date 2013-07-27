/*
It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
*/

def sortDigits(n: Int): String = n.toString.toList.sorted.mkString
val A52 = Stream.from(1).find(n => (1 to 6).map(_ * n).map(sortDigits(_)).distinct.size == 1).get
