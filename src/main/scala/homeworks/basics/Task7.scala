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

    case class PaymentCard(name: String,
                           number: String,
                           expirationDate: String,
                           securityCode: String,
                          )

    sealed trait ValidationError

    object ValidationError {

      final case object CardNameLengthError extends ValidationError {
        def errorMessage: String = "Card name should contain from 3 to 20 symbols"
      }

      final case object CardNameSpecialSymbolsError extends ValidationError {
        def errorMessage: String = "Card name can`t contain special symbols"
      }

      final case object CardNumberLengthError extends ValidationError {
        def errorMessage: String = "Card numbers should contain 16 numerals"
      }

      final case object CardNumberNumeralsError extends ValidationError {
        def errorMessage: String = "Card numbers should contain only integer digits"
      }

      final case object CardExpirationDateFormatError extends ValidationError {
        def errorMessage: String = "Card expiration date should be in format of MM/YY"
      }

      final case object CardExpirationDateMonthNumeralsError extends ValidationError {
        def errorMessage: String = "Card expiration date month value should be as numeral from 1 to 12"
      }

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

      def validateCardName(name: String): AllErrorsOr[String] = {
        def validateCardNameLength: AllErrorsOr[String] = {
          if (name.length > 2 && name.length < 21) name.validNec
          else CardNameLengthError.invalidNec
        }

        def validateCardNameSpecialSymbols(name: String): AllErrorsOr[String] = {
          if (name.matches("^[a-zA-Z]+$")) name.validNec
          else CardNameSpecialSymbolsError.invalidNec
        }

        validateCardNameLength andThen validateCardNameSpecialSymbols
      }

      def validateCardNumber(number: String): AllErrorsOr[String] = {
        def validateCardNameLength: AllErrorsOr[String] = {
          if (number.length == 16) number.validNec
          else CardNumberLengthError.invalidNec
        }

        def validateCardNameSpecialSymbols(number: String): AllErrorsOr[String] = {
          if (number.forall(_.isDigit)) number.validNec
          else CardNumberNumeralsError.invalidNec
        }

        validateCardNameLength andThen validateCardNameSpecialSymbols
      }

      def validateCardExpirationDay(expirationDate: String): AllErrorsOr[String] = {
        val dateTrimmed = expirationDate.trim.replaceAll("\\s", "")

        def validateExpirationDateFormat: AllErrorsOr[String] = {
          if (dateTrimmed.matches("\\d{2}(\\/|-)\\d{2}")) dateTrimmed.validNec
          else CardExpirationDateFormatError.invalidNec
        }

        def validateMonthAndYear(date: String): AllErrorsOr[String] = {
          val month = date.take(2).toInt
          val year = date.takeRight(2).toInt

          def validateMonth(month: Int): AllErrorsOr[String] = {
            if (month > 0 && month < 13) month.toString.validNec
            else CardExpirationDateMonthNumeralsError.invalidNec
          }

          def validateYear(year: Int): AllErrorsOr[String] = {
//                          if (year < /*actual year*/) year.toString.validNec
//                          else CardExpirationDateMonthNumeralsError.invalidNec
            year.toString.validNec//kostilj(crutch)
          }

          (validateMonth(month), validateYear(year)).mapN((month, year) => s"$month/$year")
        }

        validateExpirationDateFormat andThen validateMonthAndYear
      }

      def validateCardSecurityCode(securityCode: String): AllErrorsOr[String] = {
        def validateCardSecurityCodeLength: AllErrorsOr[String] = {
          if (securityCode.length == 3) securityCode.validNec
          else CardSecurityCodeLengthError.invalidNec
        }

        def validateCardSecurityCodeNumerals(number: String): AllErrorsOr[String] = {
          if (number.forall(_.isDigit)) number.validNec
          else CardSecurityCodeNumeralsError.invalidNec
        }

        validateCardSecurityCodeLength andThen validateCardSecurityCodeNumerals
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
          ).mapN (PaymentCard)
      }
    }

  }

}
