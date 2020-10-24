package homeworks.basics

object Task7 {

  // Homework: Error Handling
  //
  // 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.

  // Attributions and useful links:
  // https://www.lihaoyi.com/post/StrategicScalaStylePrincipleofLeastPower.html#error-handling
  // https://www.geeksforgeeks.org/scala-exception-handling/
  // https://typelevel.org/cats/datatypes/validated.html
  // https://blog.ssanj.net/posts/2019-08-18-using-validated-for-error-accumulation-in-scala-with-cats.html

  object Homework {

    sealed trait Month{
      val month: String
    }
    object Month{
      final case object January extends Month {val month: String = "01"}
      final case object February extends Month{val month: String = "02"}
      final case object March extends Month {val month: String = "03"}
      final case object April extends Month {val month: String = "04"}
      final case object May extends Month {val month: String = "05"}
      final case object June extends Month {val month: String = "06"}
      final case object July extends Month {val month: String = "07"}
      final case object August extends Month {val month: String = "08"}
      final case object September extends Month {val month: String = "09"}
      final case object October extends Month {val month: String = "10"}
      final case object November extends Month {val month: String = "11"}
      final case object December extends Month {val month: String = "12"}

      def mapper(month: String): Month = month match {
        case January.month   => January
        case February.month  => February
        case March.month     => March
        case April.month     => April
        case May.month       => May
        case June.month      => June
        case July.month      => July
        case August.month    => August
        case September.month => September
        case October.month   => October
        case November.month  => November
        case December.month  => December
      }
    }
    final case class Year(year: Int) extends AnyVal

    final case class PaymentCardName(name: String) extends AnyVal
    final case class PaymentCardNumber(number: String) extends AnyVal
    final case class PaymentCardExpirationDate(month: Month, year: Year)
    final case class PaymentCardSecurityCode(securityCode:String) extends AnyVal

    case class PaymentCard(name: PaymentCardName,
                           number: PaymentCardNumber,
                           expirationDate: PaymentCardExpirationDate,
                           securityCode: PaymentCardSecurityCode,
                          )

    sealed trait ValidationError

    object ValidationError {

      //Card Name Errors
      final case object CardNameLengthError extends ValidationError {
        def errorMessage: String = "Card name should contain from 3 to 20 symbols"
      }

      final case object CardNameSpecialSymbolsError extends ValidationError {
        def errorMessage: String = "Card name can`t contain special symbols"
      }

      //Card Number Errors
      final case object CardNumberLengthError extends ValidationError {
        def errorMessage: String = "Card numbers should contain 16 numerals"
      }

      final case object CardNumberNumeralsError extends ValidationError {
        def errorMessage: String = "Card numbers should contain only integer digits"
      }

      //Card Expiration Date Errors
      final case object CardExpirationDateFormatError extends ValidationError {
        def errorMessage: String = "Card expiration date should be in format of MM/YY"
      }

      final case object CardExpirationDateMonthNumeralsError extends ValidationError {
        def errorMessage: String = "Card expiration date month value should be as numeral from 1 to 12"
      }

      final case object CardExpirationDateYearNumeralsError extends ValidationError {
        def errorMessage: String = "Card expiration year couldnt be from the past"
      }

      //Card Security Code Errors
      final case object CardSecurityCodeLengthError extends ValidationError {
        def errorMessage: String = "Card security code should contain 3 numerals"
      }

      final case object CardSecurityCodeNumeralsError extends ValidationError {
        def errorMessage: String = "Card security code should contain only integer digits"
      }
    }

    object PaymentCardValidator {

      import ValidationError._
      import cats.data.ValidatedNec
      import cats.syntax.all._

      type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

      def validateCardName(name: String): AllErrorsOr[PaymentCardName] = {
        def validateCardNameLength: AllErrorsOr[PaymentCardName] = {
          if (name.length > 2 && name.length < 21) PaymentCardName(name).validNec
          else CardNameLengthError.invalidNec
        }

        def validateCardNameSpecialSymbols(name: String): AllErrorsOr[PaymentCardName] = {
          if (name.matches("^[a-zA-Z ]+$")) PaymentCardName(name).validNec
          else CardNameSpecialSymbolsError.invalidNec
        }

        validateCardNameLength productR validateCardNameSpecialSymbols(name)
      }

      def validateCardNumber(number: String): AllErrorsOr[PaymentCardNumber] = {
        def validateCardNameLength: AllErrorsOr[PaymentCardNumber] = {
          if (number.length == 16) PaymentCardNumber(number).validNec
          else CardNumberLengthError.invalidNec
        }

        def validateCardNameSpecialSymbols(number: String): AllErrorsOr[PaymentCardNumber] = {
          if (number.forall(_.isDigit)) PaymentCardNumber(number).validNec
          else CardNumberNumeralsError.invalidNec
        }

        validateCardNameLength productR validateCardNameSpecialSymbols(number)
      }

      def validateCardExpirationDay(expirationDate: String): AllErrorsOr[PaymentCardExpirationDate] = {
        val dateTrimmed: String = expirationDate.trim.replaceAll("\\s", "")

        def validateExpirationDateFormat: AllErrorsOr[String] = {
          if (dateTrimmed.matches("\\d{2}(\\/|-)\\d{2}") && dateTrimmed.length == 5) dateTrimmed.validNec
          else CardExpirationDateFormatError.invalidNec
        }

        def validateMonthAndYear(data: String): AllErrorsOr[PaymentCardExpirationDate] = {
          val month = data.take(2).toInt
          val year = data.takeRight(2).toInt

          def validateMonth(month: Int): AllErrorsOr[Month] = {
            if (month > 0 && month < 13) Month.mapper(month.toString).validNec
            else CardExpirationDateMonthNumeralsError.invalidNec
          }

          def validateYear(year: Int): AllErrorsOr[Year] = {
            if (year >= java.time.Year.now().toString.takeRight(2).toInt) Year(year).validNec
            else CardExpirationDateYearNumeralsError.invalidNec
          }

          if (validateMonth(month).isValid){
            if (validateYear(year).isValid){
              PaymentCardExpirationDate(validateMonth(month).toOption.get, validateYear(year).toOption.get).validNec
            }else {
              CardExpirationDateYearNumeralsError.invalidNec
            }
          }else {
            CardExpirationDateMonthNumeralsError.invalidNec
          }
        }

        validateExpirationDateFormat andThen validateMonthAndYear
      }

      def validateCardSecurityCode(securityCode: String): AllErrorsOr[PaymentCardSecurityCode] = {
        def validateCardSecurityCodeLength: AllErrorsOr[PaymentCardSecurityCode] = {
          if (securityCode.length == 3) PaymentCardSecurityCode(securityCode).validNec
          else CardSecurityCodeLengthError.invalidNec
        }

        def validateCardSecurityCodeNumerals(number: String): AllErrorsOr[PaymentCardSecurityCode] = {
          if (number.forall(_.isDigit)) PaymentCardSecurityCode(securityCode).validNec
          else CardSecurityCodeNumeralsError.invalidNec
        }

        validateCardSecurityCodeLength productR validateCardSecurityCodeNumerals(securityCode)
      }

      def validate(
                    name: String,
                    number: String,
                    expirationDate: String,
                    securityCode: String,
                  ): AllErrorsOr[PaymentCard] = {
        (validateCardName(name),
          validateCardNumber(number),
          validateCardExpirationDay(expirationDate),
          validateCardSecurityCode(securityCode)
          ).mapN(PaymentCard)
      }
    }
  }

}
