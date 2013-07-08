/*
If the numbers 1 to 5 are written out in words: one, two, three, four, five,
then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words,
how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two)
contains 23 letters and 115 (one hundred and fifteen) contains 20 letters.
The use of "and" when writing out numbers is in compliance with British usage.
*/

val oneToNine = "onetwothreefourfivesixseveneightnine".size
val tenToNineteen = "teneleventwelvethirteenfourteenfifteensixteenseventeeneighteennineteen".size
val tens = "twentythirtyfortyfiftysixtyseventyeightyninety".size
val hundredand = "hundredand".size
val and = "and".size
val oneThousand = "onethousand".size
val oneToNinetyNine = 10 * tens + 9 * oneToNine + tenToNineteen
val oneToThousand = 10 * oneToNinetyNine + 100 * oneToNine + 900 * hundredand - 9 * and + oneThousand
val A17 = oneToThousand
