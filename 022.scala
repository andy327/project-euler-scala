/*
Using names.txt, a 46K text file containing over five-thousand first names,
begin by sorting it into alphabetical order.
Then working out the alphabetical value for each name,
multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is
worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
So, COLIN would obtain a score of 938 x 53 = 49714.

What is the total of all the name scores in the file?
*/

val namesList = scala.io.Source.fromFile("names.txt").mkString.replaceAll("\"", "").split(",")
def charValue(ch: Char): Int = ch.toInt - 64
def nameValue(s: String): Int = s.toList.map(charValue(_)).sum
val A22 = namesList.sorted.zipWithIndex.map{ case (name, i) => nameValue(name) * (i + 1) }.sum
