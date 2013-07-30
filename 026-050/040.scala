/*
An irrational decimal fraction is created by concatenating the positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If d_n represents the nth digit of the fractional part, find the value of the following expression.

d_1 x d_10 x d_100 x d_1000 x d_10000 x d_100000 x d_1000000
*/

val champernowne = Stream.from(0).flatMap{ n => n.toString.map(_.asDigit) }
val A40 = List.iterate(1, 7)(_ * 10).map(champernowne(_)).product
