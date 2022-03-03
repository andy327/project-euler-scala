/*
In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

High Card: Highest value card.
One Pair: Two cards of the same value.
Two Pairs: Two different pairs.
Three of a Kind: Three cards of the same value.
Straight: All cards are consecutive values.
Flush: All cards of the same suit.
Full House: Three of a kind and a pair.
Four of a Kind: Four cards of the same value.
Straight Flush: All cards are consecutive values of same suit.
Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
The cards are valued in the order:
2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

If two players have the same ranked hands then the rank made up of the highest value wins;
for example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example,
both players have a pair of queens, then highest cards in each hand are compared (see example 4 below);
if the highest cards tie then the next highest cards are compared, and so on.

Consider the following five hands dealt to two players:

Hand    Player 1    Player 2    Winner
1   5H 5C 6S 7S KD  Pair of fives  2C 3S 8S 8D TD  Pair of Eights  Player 2
2   5D 8C 9S JS AC  Highest card Ace  2C 5C 7D 8S QH  Highest card Queen  Player 1
3   2D 9C AS AH AC  Three Aces  3D 6D 7D TD QD  Flush with Diamonds  Player 2
4   4D 6S 9H QH QC  Pair of Queens  Highest card Nine  3D 6D 7H QD QS  Pair of Queens Highest card Seven  Player 1
5   2H 2D 4C 4D 4S  Full House With Three Fours  3C 3D 3S 9S 9D Full House with Three Threes  Player 1

The file, poker.txt, contains one-thousand random hands dealt to two players.
Each line of the file contains ten cards (separated by a single space):
the first five are Player 1's cards and the last five are Player 2's cards.
You can assume that all hands are valid (no invalid characters or repeated cards),
each player's hand is in no specific order, and in each hand there is a clear winner.

How many hands does Player 1 win?
*/

object Suit {
  def apply(stringRepr: String): Suit = stringRepr match {
    case "S" => Spades
    case "H" => Hearts
    case "D" => Diamonds
    case "C" => Clubs
    case _ => sys.error(s"Invalid Suit representation: $stringRepr")
  }
}
sealed trait Suit { def toString: String }
case object Spades extends Suit { override def toString: String = "\u2660" }
case object Hearts extends Suit { override def toString: String = "\u2764" }
case object Diamonds extends Suit { override def toString: String = "\u2666" }
case object Clubs extends Suit { override def toString: String = "\u2663" }

object Card {
  def apply(stringRepr: String): Card = {
    val rank: Int = stringRepr.dropRight(1) match {
      case "A" => 14
      case "K" => 13
      case "Q" => 12
      case "J" => 11
      case "T" => 10
      case n   => n.toInt
    }
    val suit: Suit = Suit(stringRepr.takeRight(1))
    Card(rank, suit)
  }
}
case class Card(rank: Int, suit: Suit) {
  assert(rank >= 2 && rank <= 14, s"Rank can only be a value from 2 to 14 (Ace). Rank = $rank.")
  val rankString: String = rank match {
    case 14 => "A"
    case 13 => "K"
    case 12 => "Q"
    case 11 => "J"
    case 10 => "T"
    case n  => n.toString
  }
  override def toString: String = s"$rankString$suit"
}

object HandType extends Enumeration {
  val HighCard, OnePair, TwoPair, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush, RoyalFlush = Value
}

object Hand {
  import scala.math.Ordering.Implicits._
  val handOrdering: Ordering[Hand] = Ordering.by { hand: Hand => (hand.handType, hand.rankValues) }
}
case class Hand(cards: Set[Card]) extends Ordered[Hand] {
  import HandType._
  require(cards.size == 5, "Hands must comprise of 5 Cards.")

  val ranks: Set[Int] = cards.map(_.rank)
  val suits: Set[Suit] = cards.map(_.suit)
  val groups: Map[Int, Set[Card]] = cards.groupBy { card: Card => cards.count(_.rank == card.rank) }.toMap

  val isFlush: Boolean = cards.groupBy(_.suit).size == 1
  val isRoyal: Boolean = ranks == Set(14, 13, 12, 11, 10)
  val isBicycle: Boolean = ranks == Set(14, 2, 3, 4, 5)
  val isStraight: Boolean = isBicycle || (ranks.size == 5 && ranks.max - ranks.min == 4)

  val handType: HandType.Value =
    if (isRoyal && isFlush)                             RoyalFlush
    else if (isStraight && isFlush)                     StraightFlush
    else if (groups.contains(4))                        FourOfAKind
    else if (groups.contains(3) && groups.contains(2))  FullHouse
    else if (isFlush)                                   Flush
    else if (isStraight)                                Straight
    else if (groups.contains(3))                        ThreeOfAKind
    else if (groups.contains(2) && groups(2).size == 4) TwoPair
    else if (groups.contains(2))                        OnePair
    else                                                HighCard
  val rankValues: Seq[Int] = groups
    .mapValues(_.toSeq.map(_.rank).sorted.reverse) // sort within groups
    .mapValues(ranks => if (isBicycle) ranks.tail :+ ranks.head else ranks)
    .toSeq
    .sortBy(_._1).reverse // sort larger grouped ranks first
    .flatMap(_._2)

  def compare(that: Hand): Int = Hand.handOrdering.compare(this, that)

  override def toString: String = cards.mkString("[ ", ", ", " ]")
}

val pokerRounds = scala.io.Source.fromFile("poker.txt").mkString.split("\n").toList
val pokerHands = pokerRounds
  .map(_.split(" ").map(Card(_)))
  .map(allCards => (allCards.take(5).toSet, allCards.takeRight(5).toSet))
  .map { case (leftCards, rightCards) => (Hand(leftCards), Hand(rightCards)) }
val A54 = pokerHands.count { case (hand1, hand2) => hand1 > hand2 }
