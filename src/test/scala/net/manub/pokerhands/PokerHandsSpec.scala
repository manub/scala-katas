package net.manub.pokerhands

import org.scalatest.{Matchers, WordSpec}

class PokerHandsSpec extends WordSpec with Matchers {

  "Poker Hands" should {

    "recognise when the winner is White with High Card Ace" in {

      val hands = "Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C AH"

      val outcome = PokerHands.outcome(hands)

      outcome.winner should be (White)
      outcome.hand should be ("High Card: Ace")
    }

    "recognise when the winner is Black with High Card Ace" in {

      val hands = "Black: 2H 3D 5S 9C AD  White: 2C 3H 4S 8C KH"

      val outcome = PokerHands.outcome(hands)

      outcome.winner should be (Black)
      outcome.hand should be ("High Card: Ace")
    }

    "recognise a winner with High Card" in {

      val hands = "Black: 2H 3D 5S 9C JD  White: 2C 3H 4S 8C KH"

      val outcome = PokerHands.outcome(hands)

      outcome.winner should be (White)
      outcome.hand should be ("High Card: King")
    }

    "recognise a winner with a pair" in {

      val hands = "Black: 2H 3D 5S JC JD  White: 2C 3H 4S 8C KH"

      val outcome = PokerHands.outcome(hands)

      outcome.winner should be (Black)
      outcome.hand should be ("Pair: Jack")
    }

    "recognise a winner with a two of a kind" in {

      val hands = "Black: 2H 3D 5S JC JD  White: 2C 4H 4S KC KH"

      val outcome = PokerHands.outcome(hands)

      outcome.winner should be (White)
      outcome.hand should be ("Two of a Kind: King")
    }

    "recognise a winner with a three of a kind" in {

      val hands = "Black: 2H 3D JS JC JD  White: 2C 4H 4S KC KH"

      val outcome = PokerHands.outcome(hands)

      outcome.winner should be (Black)
      outcome.hand should be ("Three of a Kind: Jack")
    }

    "recognise a winner with a straight" in {
      val hands = "Black: 2H 3D JS JC JD  White: 2C 3H 4S 5C 6H"

      val outcome = PokerHands.outcome(hands)

      outcome.winner should be (White)
      outcome.hand should be ("Straight: Six")
    }

    "recognise a winner with a flush" in {
      val hands = "Black: 2D 3D JD QD KD  White: 2C 3H 4S 5C 6H"

      val outcome = PokerHands.outcome(hands)

      outcome.winner should be (Black)
      // it may be worth printing the suit?
      outcome.hand should be ("Flush: King")
    }

    "recognise a winner with a full house" in {
      val hands = "Black: 2D 3D JD QD KD  White: 2C 2H 4S 4C 4D"

      val outcome = PokerHands.outcome(hands)

      outcome.winner should be (White)
      outcome.hand should be ("Full House: Four")
    }

    "recognise a winner with four of a kind" in {
      val hands = "Black: 2D KH KS KC KD  White: 2C 2H 4S 4C 4D"

      val outcome = PokerHands.outcome(hands)

      outcome.winner should be (Black)
      outcome.hand should be ("Four of a Kind: King")
    }

    "recognise a winner with a straight flush" in {
      val hands = "Black: 2D KH KS KC KD  White: 2C 3C 4C 5C 6C"

      val outcome = PokerHands.outcome(hands)

      outcome.winner should be (White)
      outcome.hand should be ("Straight Flush: Six")
    }
  }

}
