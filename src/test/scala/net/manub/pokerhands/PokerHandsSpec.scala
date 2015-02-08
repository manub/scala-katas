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
  }

}
