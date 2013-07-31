/*
The Fibonacci sequence is defined by the recurrence relation:

F_n = F_n-1 + Fn-2, where F_1 = 1 and F_2 = 1.
Hence the first 12 terms will be:

F_1 = 1
F_2 = 1
F_3 = 2
F_4 = 3
F_5 = 5
F_6 = 8
F_7 = 13
F_8 = 21
F_9 = 34
F_10 = 55
F_11 = 89
F_12 = 144
The 12th term, F_12, is the first term to contain three digits.

What is the first term in the Fibonacci sequence to contain 1000 digits?
*/

def fib(a: BigInt, b: BigInt): Stream[BigInt] = a #:: fib(b, a + b)
val max = BigInt(10).pow(999)
val A25 = fib(0, 1).takeWhile(_ < max).size
