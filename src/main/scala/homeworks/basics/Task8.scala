package homeworks.basics


import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}

import cats.syntax.either._
import io.circe.Decoder
import io.circe.generic.JsonCodec


object Task8 {

  /**
   * HOMEWORK: JSON
   *
   * Some classes and generated JSON codecs are provided for NBA API.
   * Unfortunately, they don't work as expected out of the box.
   * The task is to fix (rewrite) some of the codecs to make tests pass.
   * You are not supposed to change anything in _class_ HomeworkSpec,
   * instead of it you are supposed to change whatever you want inside _companion object_ for HomeworkSpec.
   *
   * It would be nice to avoid using Encoder/Decoder.forProductN where you specify all field names
   */
  object HomeworkSpec {

    implicit val dateDecoder: Decoder[LocalDate] = Decoder.decodeString emap {
      input => Either
        .catchNonFatal(LocalDate.parse(input, DateTimeFormatter.BASIC_ISO_DATE))
        .leftMap (err => s"$err is not parsable date value")
    }

    implicit val teamTotalsDecoder: Decoder[TeamTotals] = Decoder.instance{ ins =>
      for{
        assists               <-  ins.get[String]("assists")
        fullTimeoutRemaining  <-  ins.get[String]("full_timeout_remaining")
        plusMinus             <-  ins.get[String]("plusMinus")
      } yield TeamTotals(assists, fullTimeoutRemaining, plusMinus)
    }

    @JsonCodec final case class TeamTotals(assists: String, fullTimeoutRemaining: String, plusMinus: String)

    @JsonCodec final case class TeamBoxScore(totals: TeamTotals)

    @JsonCodec final case class GameStats(hTeam: TeamBoxScore, vTeam: TeamBoxScore)

    @JsonCodec final case class PrevMatchup(gameDate: LocalDate, gameId: String)

    @JsonCodec final case class BoxScore(
                                          basicGameData: Game,
                                          previousMatchup: PrevMatchup,
                                          stats: Option[GameStats],
                                        )

    @JsonCodec final case class JustScore(score: String)

    @JsonCodec final case class TeamStats(
                                           linescore: List[JustScore],
                                           loss: String,
                                           score: String,
                                           teamId: String,
                                           triCode: String
                                         )

    @JsonCodec final case class GameDuration(hours: String, minutes: String)

    @JsonCodec final case class Arena(
                                       city: String,
                                       country: String,
                                       isDomestic: Boolean,
                                       name: String,
                                       stateAbbr: String
                                     )

    @JsonCodec final case class Game(
                                      arena: Arena,
                                      attendance: String,
                                      endTimeUTC: Option[ZonedDateTime],
                                      gameDuration: GameDuration,
                                      gameId: String,
                                      gameUrlCode: String,
                                      hTeam: TeamStats,
                                      isBuzzerBeater: Boolean,
                                      startTimeUTC: ZonedDateTime,
                                      vTeam: TeamStats,
                                    )

    @JsonCodec final case class Scoreboard(games: List[Game], numGames: Int)

  }

}
