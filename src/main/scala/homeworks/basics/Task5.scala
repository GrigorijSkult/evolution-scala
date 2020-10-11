package homeworks.basics

import homeworks.basics.Task3.ErrorMessage
import homeworks.basics.Task5.HoldemType.{Omaha, Texas}

object Task5 {

  // Homework: AlgebraicDataTypes
  //
  // Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  // Attributions and useful links:
  // https://nrinaudo.github.io/scala-best-practices/definitions/adt.html
  // https://alvinalexander.com/scala/fp-book/algebraic-data-types-adts-in-scala/
  // https://en.wikipedia.org/wiki/Algebraic_data_type

  //0. Holdem type
  sealed trait HoldemType
  object HoldemType {
    case object Texas extends HoldemType
    case object Omaha extends HoldemType
  }

  //1. Suit
  final case class Suit(suitType: Char)
  object Suit {
    def mapper(suitType: Char): Either[ErrorMessage, Suit] = {
      suitType match {
        case 'c' | 'd' | 'h' | 's'  => Right(Suit(suitType))
        case _                      => Left(ErrorMessage("Error: Invalid card suit type"))
      }
    }
  }

  //2. Rank
  final case class Rank(rankType: Byte)
  object Rank {
    def mapper(rankType: Char): Either[ErrorMessage, Rank] = {
      rankType match {
        case '2'   => Right(Rank(2))
        case '3'   => Right(Rank(3))
        case '4'   => Right(Rank(4))
        case '5'   => Right(Rank(5))
        case '6'   => Right(Rank(6))
        case '7'   => Right(Rank(7))
        case '8'   => Right(Rank(8))
        case '9'   => Right(Rank(9))
        case 'T'   => Right(Rank(10))
        case 'J'   => Right(Rank(11))
        case 'Q'   => Right(Rank(12))
        case 'K'   => Right(Rank(13))
        case 'A'   => Right(Rank(14))
        case _     => Left(ErrorMessage("Error: Invalid card rank type"))
      }
    }
  }

  // 3. Card
  case class Card(rank: Rank, suit: Suit, kicker: Boolean)
  //kicker:Boolean may be useful in omaha algorithm
  //check of the rank and suit input(Right) should be performed before creating a new card

  // 4. Hand (Texas or Omaha)
  final case class Hand(handCards: List[Card])
  //handStrengths will be needed for hand sorting
  object Hand {
    def create(handCards: List[Card], holdemType: HoldemType): Either[ErrorMessage, Hand] = {
      holdemType match {
        case Texas                    =>
          if (handCards.length == 2)  Right(Hand(handCards))
          else                        Left(ErrorMessage("Error: Invalid hand creation"))
        case Omaha =>
          if (handCards.length == 4)  Right(Hand(handCards))
          else                        Left(ErrorMessage("Error: Invalid hand creation"))
        case _                        => Left(ErrorMessage("Error: Invalid hand creation"))
      }
    }
  }

  // 5. Board
  final case class Board(handCards: List[Card])
  object Board {
    def create(boardCards: List[Card]): Either[ErrorMessage, Board] = {
      if (boardCards.length == 5)   Right(Board(boardCards))
      else                          Left(ErrorMessage("Error: Invalid board creation"))
    }
  }

  // 6. Poker Combination (High Card, Pair, etc.)
  sealed trait PokerCombination {
    var combinationStrengths: Int //for easiest sorting in case of equal strengths combinations
    val combinationCards: List[Card]
    val handCards: Hand
  }
  object PokerCombination {

    case class HigherCard(cards: List[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 1
      val combinationCards: List[Card] = cards
      val handCards: Hand = hand
    }

    case class Pair(cards: List[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 2
      val combinationCards: List[Card] = cards
      val handCards: Hand = hand
    }

    case class TwoPair(cards: List[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 3
      val combinationCards: List[Card] = cards
      val handCards: Hand = hand
    }

    case class ThreeOfAKind(cards: List[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 4
      val combinationCards: List[Card] = cards
      val handCards: Hand = hand
    }

    case class Straight(cards: List[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 5
      val combinationCards: List[Card] = cards
      val handCards: Hand = hand
    }

    case class Flash(cards: List[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 6
      val combinationCards: List[Card] = cards
      val handCards: Hand = hand
    }

    case class FullHouse(cards: List[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 7
      val combinationCards: List[Card] = cards
      val handCards: Hand = hand
    }

    case class FourOfAKind(cards: List[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 8
      val combinationCards: List[Card] = cards
      val handCards: Hand = hand
    }

    case class StraightFlush(cards: List[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 9
      val combinationCards: List[Card] = cards
      val handCards: Hand = hand
    }

    case class RoyalStraightFlush(cards: List[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 10
      val combinationCards: List[Card] = cards
      val handCards: Hand = hand
    }

  }

  //--. find best card combinations,
  //perform PokerCombination sorting by combinationStrengths
  case class CombinationCalculation(board: Board, hands: List[Hand]){
    def calculation: List[PokerCombination] = ???
  }

  // 7. Test Case (Board & Hands to rank)
  //Test Case = one parsed line of input in the input from stdin
  case class testCase(inputString: String){
    def calculation: PokerCombination = ???
  }

  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //Test Result = one non-rendered line of output in the output to stdout
  case class testResult(board: Board, hands: List[Hand]){
    CombinationCalculation.apply(board, hands).calculation
  }

}
