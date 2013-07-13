/*
The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
*/

def selfPowerModulus(n: Int, m: Long): Long = Stream.iterate(n.toLong)(_ * n % m)(n - 1)
val A48 = (1 to 1000).map(n => selfPowerModulus(n, 10000000000L)).sum % 10000000000L
