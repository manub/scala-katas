package net.manub.pokerhands

object PokerHands {

  val handsPattern = "Black: (.*?)  White: (.*?)".r

  def outcome(hands: String): GameOutcome = {

    val (blackHand, whiteHand) = hands match {
      case handsPattern(cardsBlackPlayer, cardsWhitePlayer) =>
        (Hand(cardsBlackPlayer), Hand(cardsWhitePlayer))
    }

    val winningPlayer: Player =
      if (blackHand.cards.exists(_.value == ACE)) Black else White

    GameOutcome(winningPlayer, "High Card: Ace")
  }

}

case class Hand(cards: Seq[Card])

object Hand {

  def apply(hand: String): Hand = {
    val cards = hand.split(" ")
    require(cards.size == 5)
    Hand(cards.map(card => Card(card)))
  }
}

case class Card(value: Value, suit: Suit)

object Card {

  def apply(card: String): Card = {
    require(card.length == 2)
    Card(Value.from(card(0)), Suit.from(card(1)))
  }
}

abstract class Value

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
}

case object TWO   extends Value
case object THREE extends Value
case object FOUR  extends Value
case object FIVE  extends Value
case object SIX   extends Value
case object SEVEN extends Value
case object EIGHT extends Value
case object NINE  extends Value
case object JACK  extends Value
case object QUEEN extends Value
case object KING  extends Value
case object ACE   extends Value

abstract class Suit

object Suit {

  def from(suit: Char): Suit = suit match {
    case 'H' => HEARTS
    case 'D' => DIAMONDS
    case 'C' => CLUBS
    case 'S' => SPADES
  }
}

case object HEARTS extends Suit
case object DIAMONDS extends Suit
case object CLUBS extends Suit
case object SPADES extends Suit


case class GameOutcome(winner: Player, hand: String)

trait Player {
  def name: String
}

case object Black extends Player { val name = "Black" }
case object White extends Player { val name = "White" }