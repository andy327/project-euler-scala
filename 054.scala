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

import scala.collection.SetLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{ Builder, SetBuilder }

sealed abstract class Suit
case object Spades extends Suit
case object Hearts extends Suit
case object Diamonds extends Suit
case object Clubs extends Suit

case class Card(value: Int, suit: Suit) {
  assert(value >= 1 && value <= 13)
  private val valueRepr: String = value match {
    case 13 => "K"
    case 12 => "Q"
    case 11 => "J"
    case 1 => "A"
    case n => n.toString
  }
  private val suitRepr: String = suit match {
    case Spades => "S"
    case Hearts => "H"
    case Diamonds => "D"
    case Clubs => "C"
  }
  override def toString: String = valueRepr.concat(suitRepr)
}

object Card {
  def from(card: String): Card = {
    val suit = card.last.toString match {
      case "S" => Spades
      case "H" => Hearts
      case "D" => Diamonds
      case "C" => Clubs
    }
    val value = card.dropRight(1) match {
      case "K" => 13
      case "Q" => 12
      case "J" => 11
      case "T" => 10
      case "A" => 1
      case n => n.toInt
    }
    new Card(value, suit)
  }
}

class Hand(seq: Card*) extends Set[Card] with SetLike[Card, Hand] with Ordered[Hand] {
  override def empty: Hand = new Hand()
  val values: Seq[Int] = seq.map(_.value)
  val suits: Seq[Suit] = seq.map(_.suit)
  val valueCounts: Iterable[Int] = values.groupBy(identity).mapValues(_.size).values
  val suitCounts: Iterable[Int] = suits.groupBy(identity).mapValues(_.size).values
  lazy val valueCardMap: Map[Int, Hand] = seq.groupBy(_.value).map(h => h._1 -> Hand(h._2: _*))
  lazy val rankedCardMap: Map[Int, Hand] = if (valueCardMap.keySet(1)) valueCardMap + (14 -> valueCardMap(1)) - 1 else valueCardMap
  lazy val suitCardMap: Map[Suit, Hand] = seq.groupBy(_.suit).map(h => h._1 -> Hand(h._2: _*))
  lazy val handType: HandType = Hand.types.find(_.foundIn(this)).getOrElse(Empty)
  lazy val score: Int = handType.score(this)
  def bestHand: Hand = handType.bestHand(this).get
  def highValue: Int = rankedCardMap.maxBy(_._1)._1
  def has(hType: HandType): Boolean = hType.foundIn(this)
  def hasNo(hType: HandType): Boolean = !has(hType)
  def beats(hType: HandType): Boolean = handType.beats(hType)
  def beats(that: Hand): Boolean = this > that
  def compare(that: Hand) = this.score - that.score
  def contains(elem: Card): Boolean = seq.exists(elem == _)
  def iterator: Iterator[Card] = seq.iterator
  def +(elem: Card): Hand = if (seq contains elem) this else new Hand(elem +: seq: _*)
  def -(elem: Card): Hand = if (!(seq contains elem)) this else new Hand(seq filterNot (elem == _): _*)
  override def toString: String = seq.mkString("[", " ", "]")
}

object Hand {
  def empty: Hand = new Hand()
  def types = List(StraightFlush, FourKind, FullHouse, Flush, Straight, ThreeKind, TwoPair, Pair, HighCard, Empty)
  def newBuilder: Builder[Card, Hand] = new SetBuilder[Card, Hand](empty)
  def apply(elems: Card*): Hand = (empty /: elems)(_ + _)
  def apply(elems: Set[Card]): Hand = (empty /: elems)(_ + _)
  def HandCanBuildFromHand = new CanBuildFrom[Hand, Card, Hand] {
    def apply(from: Hand) = newBuilder
    def apply() = newBuilder
  }
  def HandCanBuildFromSet = new CanBuildFrom[Set[Card], Card, Hand] {
    def apply(from: Set[Card]) = newBuilder
    def apply() = newBuilder
  }
  def from(hand: String): Hand = Hand.apply(hand.split(" ").map(Card.from(_)).toSet)
  def from(hand: Seq[String]): Hand = Hand.from(hand.mkString(" "))
}

sealed abstract class HandType {
  protected def typeScore: Int
  protected def handScore(hand: Hand): Int
  def score(hand: Hand): Int = if (notFoundIn(hand)) 0 else typeScore * 1E7.toInt + handScore(hand)
  def beats(hType: HandType): Boolean = typeScore > hType.typeScore
  def foundIn(hand: Hand): Boolean
  def notFoundIn(hand: Hand): Boolean = !foundIn(hand)
  def bestHand(hand: Hand): Option[Hand] = if (foundIn(hand)) Some(getBestHand(hand)) else None
  protected def getBestHand(hand: Hand): Hand
}

case object Empty extends HandType {
  val typeScore: Int = 0
  def handScore(hand: Hand): Int = 0
  def foundIn(hand: Hand): Boolean = true
  def getBestHand(hand: Hand): Hand = Hand.empty
}

case object HighCard extends HandType {
  val typeScore: Int = 1
  def handScore(hand: Hand): Int = if (hand.values.contains(1)) 14 else hand.values.max
  def foundIn(hand: Hand): Boolean = !hand.isEmpty
  def getBestHand(hand: Hand): Hand = Hand(hand.rankedCardMap.get(hand.highValue).head.head)
}

case object Pair extends HandType {
  val typeScore: Int = 2
  def handScore(hand: Hand): Int = hand.rankedCardMap.filter(_._2.size >= 2).keySet.max
  def foundIn(hand: Hand): Boolean = !hand.valueCounts.filter(_ >= 2).isEmpty
  def getBestHand(hand: Hand): Hand = hand.rankedCardMap.filter(_._2.size >= 2).maxBy(_._1)._2.take(2)
}

case object TwoPair extends HandType {
  val typeScore: Int = 3
  def handScore(hand: Hand): Int = {
    val top2: List[Int] = hand.rankedCardMap.filter(_._2.size >= 2).keySet.toList.sorted.takeRight(2)
    (top2(0) + top2(1) * 14)
  }
  def foundIn(hand: Hand): Boolean = hand.valueCounts.filter(_ >= 2).size >= 2
  def getBestHand(hand: Hand): Hand = {
    val top2: List[Hand] = hand.rankedCardMap.filter(_._2.size >= 2).toList.sortBy(_._1).takeRight(2).map(_._2.take(2))
    top2(0) ++ top2(1)
  }
}

case object ThreeKind extends HandType {
  val typeScore: Int = 4
  def handScore(hand: Hand): Int = hand.rankedCardMap.filter(_._2.size >= 3).keySet.max
  def foundIn(hand: Hand): Boolean = !hand.valueCounts.filter(_ >= 3).isEmpty
  def getBestHand(hand: Hand): Hand = hand.rankedCardMap.filter(_._2.size >= 3).maxBy(_._1)._2.take(3)
}

case object Straight extends HandType {
  val typeScore: Int = 5
  def handScore(hand: Hand): Int = getBestHand(hand) match {
    case h if h.values.toSet == Set(1, 2, 3, 4, 5) => 5
    case h => if (h.values.contains(1)) 14 else h.values.max
  }
  private def straights(hand: Hand): List[Seq[Int]] = {
    val values: Seq[Int] = hand.values.distinct.sorted
    val matches: Seq[Int] = if (values.contains(1)) values.sorted :+ 14 else values.sorted
    matches.sliding(5, 1).filter(_.size >= 5).filter(five => five(4) - five(0) == 4).toList
  }
  def foundIn(hand: Hand): Boolean = !straights(hand).isEmpty
  def getBestHand(hand: Hand): Hand =
    Hand(hand.valueCardMap.filter(kv => straights(hand).maxBy(_.max).contains(kv._1)).mapValues(_.head).values.toSeq: _*)
}

case object Flush extends HandType {
  val typeScore: Int = 6
  def handScore(hand: Hand): Int = rankFlush(getBestHand(hand))
  private def flushes(hand: Hand): Iterable[Hand] = hand.suitCardMap.filter(_._2.size >= 5)
    .mapValues(cards => Hand(cards.toSeq: _*)).values
  private def rankFlush(hand: Hand): Int = hand.values.map(math.pow(2, _)).sum.toInt
  def foundIn(hand: Hand): Boolean = !hand.suitCounts.filter(_ >= 5).isEmpty
  def getBestHand(hand: Hand): Hand = flushes(hand).maxBy(rankFlush(_))
}

case object FullHouse extends HandType {
  val typeScore: Int = 7
  def handScore(hand: Hand): Int = getBestHand(hand).rankedCardMap.map(
    kv => kv._2.size match {
      case 3 => 14 * kv._1
      case 2 => kv._1
      }).sum
  def foundIn(hand: Hand): Boolean = hand.valueCounts.filter(_ >= 2).toSet.size >= 2
  def getBestHand(hand: Hand): Hand = {
    val bestTriple: Hand = hand.rankedCardMap.filter(_._2.size >= 3).maxBy(_._1)._2.take(3)
    val bestPair: Hand = hand.rankedCardMap.filterKeys(_ != bestTriple.highValue).filter(_._2.size >= 2).maxBy(_._1)._2.take(2)
    bestTriple ++ bestPair
  }
}

case object FourKind extends HandType {
  val typeScore: Int = 8
  def handScore(hand: Hand): Int = hand.rankedCardMap.filter(_._2.size >= 4).keySet.max
  def foundIn(hand: Hand): Boolean = !hand.valueCounts.filter(_ >= 4).isEmpty
  def getBestHand(hand: Hand): Hand = hand.rankedCardMap.filter(_._2.size >= 4).maxBy(_._1)._2
}

case object StraightFlush extends HandType {
  val typeScore: Int = 9
  def handScore(hand: Hand): Int = rankFlush(getBestHand(hand))
  private def rankFlush(hand: Hand): Int = hand.values.map(math.pow(2, _)).sum.toInt
  private def flushes(hand: Hand): Iterable[Hand] = hand.suitCardMap.filter(_._2.size >= 5)
    .mapValues(cards => Hand(cards.toSeq: _*)).values
  private def straightFlushes(hand: Hand): Iterable[Hand] = flushes(hand).filter(Straight.foundIn(_)).map(Straight.getBestHand(_))
  def foundIn(hand: Hand): Boolean = !straightFlushes(hand).isEmpty
  def getBestHand(hand: Hand): Hand = straightFlushes(hand).maxBy(rankFlush(_))
}

val pokerRounds = scala.io.Source.fromFile("poker.txt").mkString.split("\n").toList
val pokerHands = pokerRounds.map(_.split(" ")).map(all => (Hand.from(all.take(5)), Hand.from(all.takeRight(5))))
val A54 = pokerHands.filter(hs => hs._1 > hs._2).size
