/*
Take the number 192 and multiply it by each of 1, 2, and 3:

192 x 1 = 192
192 x 2 = 384
192 x 3 = 576

By concatenating each product we get the 1 to 9 pandigital, 192384576.
We will call 192384576 the concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5,
giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the
concatenated product of an integer with (1,2, ... , n) where n > 1?
*/

def isPandigital(s: String): Boolean = (1 to 9).forall(d => s.contains(d.toString) && s.size == 9)
def pandigitalMultiple(n: Int): Option[Int] = (1 to 9).map(n * _).scanLeft("")(_ + _.toString).drop(1).find(isPandigital(_)).map(_.toInt)
val A38 = (1 to 9999).map(pandigitalMultiple(_)).filter(_.isDefined).max.get
