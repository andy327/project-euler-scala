/*
You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
*/

val nonLeapOffsets = List(0, 3, 3, 6, 1, 4, 6, 2, 5, 0, 3, 5)
val leapOffsets = List(0, 3, 4, 0, 2, 5, 0, 3, 6, 1, 4, 6)
def shiftMonths(offsets: List[Int], shift: Int): List[Int] = offsets.map(first => (first + shift) % 7)
def isLeapYear(year: Int): Boolean = year % 4 == 0 && year % 400 != 0
def countSundays(year: Int) = {
  val offsets = if (isLeapYear(year)) leapOffsets else nonLeapOffsets
  shiftMonths(offsets, yearOffset(year)).count(_ == 0)
}
def yearOffset(year: Int): Int = {
  require(year > 0)
  val cycleOffset = year % 400
  val centuryOffset = cycleOffset % 100
  val centuriesPast = (cycleOffset - 1) / 100
  val leapsPast = (cycleOffset - 1) / 4 - centuriesPast
  val offset = 6 + cycleOffset + leapsPast
  if (cycleOffset == 0) 6 else 1 + offset % 7
}
val A19 = (1901 to 2000).map(countSundays(_)).sum
