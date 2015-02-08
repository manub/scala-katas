package net.manub.pokerhands

import scala.util.Try

object PokerHands {

  val handsPattern = "Black: (.*?)  White: (.*?)".r

  def outcome(hands: String): GameOutcome = {

    val (blackHand, whiteHand) = hands match {
      case handsPattern(cardsBlackPlayer, cardsWhitePlayer) =>
        (Hand(cardsBlackPlayer), Hand(cardsWhitePlayer))
    }

    val (blackHigherScore, blackWinningValue) = blackHand.higherScore
    val (whiteHigherScore, whiteWinningValue) = whiteHand.higherScore

    if (blackHigherScore > whiteHigherScore || (blackHigherScore == whiteHigherScore && blackWinningValue > whiteWinningValue)) {
      GameOutcome(Black, s"${blackHigherScore.name}: ${blackWinningValue.name}")
    } else {
      GameOutcome(White, s"${whiteHigherScore.name}: ${whiteWinningValue.name}")
    }
  }

}

trait Score extends Ordered[Score] {

  def matches(hand: Hand): Option[Value]

  def name: String

  override def compare(that: Score): Int =
    Scores.all.indexOf(this) compare Scores.all.indexOf(that)

  protected def maximumValueWithAtLeastOccurrences(cards: Seq[Card], numberOfMinimumOccurrences: Int): Option[Value] = {
    Try(cards
      .groupBy(_.value)
      .mapValues(_.size)
      .filter { case (_, occurrences) => occurrences >= numberOfMinimumOccurrences}
      .keys
      .max
    ).toOption
  }
}

object Scores {
  lazy val all = Seq(HIGHER_CARD, PAIR, TWO_OF_A_KIND, THREE_OF_A_KIND)
}

case object HIGHER_CARD extends Score {

  override def matches(hand: Hand) = Some(hand.higherCard.value)

  override def name = "High Card"
}

case object PAIR extends Score {

  override def matches(hand: Hand) = maximumValueWithAtLeastOccurrences(hand.cards, 2)

  override def name = "Pair"
}

case object TWO_OF_A_KIND extends Score {

  override def matches(hand: Hand): Option[Value] = for {
      maximumValuePresentInFirstPair <- maximumValueWithAtLeastOccurrences(hand.cards, 2)
      maximumValuePresentInSecondPair <- maximumValueWithAtLeastOccurrences(hand.cards.filterNot(card => card.value == maximumValuePresentInFirstPair), 2)
    } yield maximumValuePresentInFirstPair

  override def name: String = "Two of a Kind"
}

case object THREE_OF_A_KIND extends Score {

  override def matches(hand: Hand) = maximumValueWithAtLeastOccurrences(hand.cards, 3)

  override def name: String = "Three of a Kind"
}

trait Player {
  def name: String
}

case object Black extends Player {
  val name = "Black"
}

case object White extends Player {
  val name = "White"
}

case class Hand(cards: Seq[Card]) {

  def higherCard: Card = cards.max

  def higherScore: (Score, Value) = {
    Scores.all
      .reverse
      .map(score => (score, score.matches(this)))
      .filter { case (score, maybeMatched) => maybeMatched.isDefined }
      .map { case (score, matched) => (score, matched.get)}
      .head
  }
}

object Hand {

  def apply(hand: String): Hand = {
    val cards = hand.split(" ")
    require(cards.size == 5)
    Hand(cards.map(card => Card(card)))
  }
}

case class Card(value: Value, suit: Suit) extends Ordered[Card] {
  override def compare(that: Card): Int = value compare that.value
}

object Card {
  def apply(card: String): Card = {
    require(card.length == 2)
    Card(Value.from(card(0)), Suit.from(card(1)))
  }
}

abstract class Value(val name: String) extends Ordered[Value] {
  override def compare(that: Value): Int =
    Value.order.indexOf(this) compare Value.order.indexOf(that)
}

object Value {

  def from(value: Char): Value = value match {
    case '2' => TWO
    case '3' => THREE
    case '4' => FOUR
    case '5' => FIVE
    case '6' => SIX
    case '7' => SEVEN
    case '8' => EIGHT
    case '9' => NINE
    case 'J' => JACK
    case 'Q' => QUEEN
    case 'K' => KING
    case 'A' => ACE
  }

  lazy val order = Seq(TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, JACK, QUEEN, KING, ACE)
}

case object TWO extends Value("Two")
case object THREE extends Value("Three")
case object FOUR extends Value("Four")
case object FIVE extends Value("Five")
case object SIX extends Value("Six")
case object SEVEN extends Value("Seven")
case object EIGHT extends Value("Eight")
case object NINE extends Value("Nine")
case object JACK extends Value("Jack")
case object QUEEN extends Value("Queen")
case object KING extends Value("King")
case object ACE extends Value("Ace")

abstract class Suit(val name: String)

object Suit {

  def from(suit: Char): Suit = suit match {
    case 'H' => HEARTS
    case 'D' => DIAMONDS
    case 'C' => CLUBS
    case 'S' => SPADES
  }
}

case object HEARTS extends Suit("Hearts")

case object DIAMONDS extends Suit("Diamonds")

case object CLUBS extends Suit("Clubs")

case object SPADES extends Suit("Spades")

case class GameOutcome(winner: Player, hand: String)

