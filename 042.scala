/*
The nth term of the sequence of triangle numbers is given by, t_n = 1/2n(n+1);
so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical position
and adding these values we form a word value.
For example, the word value for SKY is 19 + 11 + 25 = 55 = t10.
If the word value is a triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing
nearly two-thousand common English words, how many are triangle words?
*/

val wordsList = scala.io.Source.fromFile("words.txt").mkString.replaceAll("\"", "").split(",")
def charValue(ch: Char): Int = ch.toInt - 64
def wordValue(s: String): Int = s.toList.map(charValue(_)).sum
def isTriangular(r: Int): Boolean = {
  val n = scala.math.sqrt(2 * r).toInt
  n * (n + 1) == 2 * r
}
val A42 = wordsList.count(w => isTriangular(wordValue(w)))
