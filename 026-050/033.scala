/*
The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting
to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in value,
and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms,
find the value of the denominator.
*/

val candidates = for (a <- (1 to 9); b <- (1 to 9)) yield (a, b, (10 * a * b / (9 * a + b)))
val curiousFractions = candidates.filter(abc => { val (a,b,c) = abc; (10 * a + b) * c == (10 * b + c) * a && a < c } )
val A33 = curiousFractions.map(abc => { val (a,b,c) = abc; c.toDouble / a }).product.toInt
