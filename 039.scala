/*
If p is the perimeter of a right angle triangle with integral length sides, {a,b,c},
there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p <= 1000, is the number of solutions maximised?
*/

def isPythagoreanTriple = (a: Int, b: Int, c: Int) => a*a + b*b == c*c
def pythagoreanTriples(p: Int): Set[(Int, Int, Int)] = {
  val candidates = for(a <- (1 to p / 3); b <- (a to p / 2)) yield (a, b, p-a-b)
  candidates.filter(isPythagoreanTriple.tupled(_)).toSet
}
val A39 = (1 to 1000).maxBy(pythagoreanTriples(_).size)
