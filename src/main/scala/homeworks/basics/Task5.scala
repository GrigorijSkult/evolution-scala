package homeworks.basics

import cats.data.{NonEmptyList, NonEmptySet}
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
  sealed trait Suit{
    val suit: String
  }
  object Suit {
    final case object Club extends Suit {val suit = "h"}
    final case object Diamond extends Suit {val suit = "d"}
    final case object Heart extends Suit {val suit = "c"}
    final case object Spade extends Suit {val suit = "s"}

    def mapper(suitType: String): Either[ErrorMessage, Suit] = {
      suitType match {
        case "c"  => Right(Club)
        case "d"  => Right(Diamond)
        case "h"  => Right(Heart)
        case "s"  => Right(Spade)
        case _    => Left(ErrorMessage("Error: Invalid card suit type"))
      }
    }
  }

  //2. Rank
  sealed trait Rank{
    val rank: String
    val strength: Int
  }
  object Rank {
    final case object Two extends Rank {val rank = "2"; val strength = 2}
    final case object Three extends Rank {val rank = "3"; val strength = 3}
    final case object Four extends Rank {val rank = "4"; val strength = 4}
    final case object Five extends Rank {val rank = "5"; val strength = 5}
    final case object Six extends Rank {val rank = "6"; val strength = 6}
    final case object Seven extends Rank {val rank = "7"; val strength = 7}
    final case object Eight extends Rank {val rank = "8"; val strength = 8}
    final case object Nine extends Rank {val rank = "9"; val strength = 9}
    final case object T extends Rank {val rank = "T"; val strength = 10}
    final case object J extends Rank {val rank = "J"; val strength = 11}
    final case object Q extends Rank {val rank = "Q"; val strength = 12}
    final case object K extends Rank {val rank = "K"; val strength = 13}
    final case object A extends Rank {val rank = "A"; val strength = 14}

    def mapper(rankType: String): Either[ErrorMessage, Rank] = {
      rankType match {
        case "2"   => Right(Two)
        case "3"   => Right(Three)
        case "4"   => Right(Four)
        case "5"   => Right(Five)
        case "6"   => Right(Six)
        case "7"   => Right(Seven)
        case "8"   => Right(Eight)
        case "9"   => Right(Nine)
        case "T"   => Right(T)
        case "J"   => Right(J)
        case "Q"   => Right(Q)
        case "K"   => Right(K)
        case "A"   => Right(T)
        case _     => Left(ErrorMessage("Error: Invalid card rank type"))
      }
    }
  }

  // 3. Card
  case class Card(rank: Rank, suit: Suit, kicker: Boolean)
  //kicker:Boolean may be useful in omaha algorithm
  //check of the rank and suit input(Right) should be performed before creating a new card

  // 4. Hand (Texas or Omaha)
  final case class Hand(handCards: Set[Card])
  object Hand {
    def create(handCards: Set[Card], holdemType: HoldemType): Either[ErrorMessage, Hand] = {
      holdemType match {
        case Texas                  =>
          if (handCards.size == 2)  Right(Hand(handCards))
          else                      Left(ErrorMessage("Error: Invalid hand creation"))
        case Omaha =>
          if (handCards.size == 4)  Right(Hand(handCards))
          else                      Left(ErrorMessage("Error: Invalid hand creation"))
        case _                      => Left(ErrorMessage("Error: Invalid hand creation"))
      }
    }
  }

  // 5. Board
  final case class Board(handCards: Set[Card])
  object Board {
    def create(boardCards: Set[Card]): Either[ErrorMessage, Board] = {
      if (boardCards.size == 5)   Right(Board(boardCards))
      else                          Left(ErrorMessage("Error: Invalid board creation"))
    }
  }

  // 6. Poker Combination (High Card, Pair, etc.)
  sealed trait PokerCombination {
    var combinationStrengths: Int //for easiest sorting in case of equal strengths combinations
    val combinationCards: Set[Card]
    val handCards: Hand
  }
  object PokerCombination {

    case class HigherCard(cards: Set[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 1
      val combinationCards: Set[Card] = cards
      val handCards: Hand = hand
    }

    case class Pair(cards: Set[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 2
      val combinationCards: Set[Card] = cards
      val handCards: Hand = hand
    }

    case class TwoPair(cards: Set[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 3
      val combinationCards: Set[Card] = cards
      val handCards: Hand = hand
    }

    case class ThreeOfAKind(cards: Set[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 4
      val combinationCards: Set[Card] = cards
      val handCards: Hand = hand
    }

    case class Straight(cards: Set[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 5
      val combinationCards: Set[Card] = cards
      val handCards: Hand = hand
    }

    case class Flash(cards: Set[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 6
      val combinationCards: Set[Card] = cards
      val handCards: Hand = hand
    }

    case class FullHouse(cards: Set[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 7
      val combinationCards: Set[Card] = cards
      val handCards: Hand = hand
    }

    case class FourOfAKind(cards: Set[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 8
      val combinationCards: Set[Card] = cards
      val handCards: Hand = hand
    }

    case class StraightFlush(cards: Set[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 9
      val combinationCards: Set[Card] = cards
      val handCards: Hand = hand
    }

    case class RoyalStraightFlush(cards: Set[Card], hand: Hand) extends PokerCombination {
      var combinationStrengths = 10
      val combinationCards: Set[Card] = cards
      val handCards: Hand = hand
    }

  }

  //--. find best card combinations,
  //perform PokerCombination sorting by combinationStrengths
  case class CombinationCalculation(board: Board, hands: NonEmptySet[Hand]){
    def calculation: Set[PokerCombination] = ???
  }

  // 7. Test Case (Board & Hands to rank)
  //Test Case = one parsed line of input in the input from stdin
  case class testCase(inputString: String){
    def calculation: PokerCombination = ???
  }

  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //Test Result = one non-rendered line of output in the output to stdout
  case class testResult(board: Board, hands: NonEmptySet[Hand]){
    CombinationCalculation.apply(board, hands).calculation
  }

}
