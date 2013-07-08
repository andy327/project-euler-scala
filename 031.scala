/*
In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

1£1 + 150p + 220p + 15p + 12p + 31p

How many different ways can £2 be made using any number of coins?
*/

val coins = Set(1, 2, 5, 10, 20, 50, 100, 200)
def possibilities(total: Int, coinsLeft: Set[Int]): Int = total match {
  case 0 => 1
  case _ if (coinsLeft.size == 0) => 0
  case _ if (coinsLeft == Set(1)) => 1
  case _ => {
    val usableCoins = coinsLeft.filter(_ <= total)
    val biggestCoin = usableCoins.max
    possibilities(total, usableCoins - biggestCoin)
      + possibilities(total - biggestCoin, usableCoins)
  }
}
val A31 = possibilities(200, coins)
