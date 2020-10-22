package homeworks.basics

import cats.data.Chain
import cats.syntax.all._
import homeworks.basics.Task7.Homework.ValidationError._
import homeworks.basics.Task7.Homework._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Task7Test extends AnyFreeSpec with Matchers {

  val yearTwoLastDigits: String = java.time.Year.now().getValue.toString.takeRight(2)

  "CardValidation" - {
    "be valid" in {
      PaymentCardValidator.validate("Gregory", "1231231215426154", "12/" + yearTwoLastDigits, "000") shouldBe
      PaymentCard("Gregory", "1231231215426154", "12/" + yearTwoLastDigits, "000").validNec
    }

    "be invalid" in {
      PaymentCardValidator.validate("X", "0", "21/" + (java.time.Year.now().getValue - 1).toString.takeRight(2), "Vladimir").toString shouldBe
      "Invalid(Chain(CardNameLengthError, CardNumberLengthError, CardExpirationDateMonthNumeralsError, CardExpirationDateYearNumeralsError, CardSecurityCodeLengthError, CardSecurityCodeNumeralsError))"
      //        (CardNameLengthError, CardNumberLengthError, CardExpirationDateMonthNumeralsError, CardSecurityCodeLengthError).invalidNec // space problem
    }
  }

  "CardNameValidation" - {
    "valid length" in {
      PaymentCardValidator.validateCardName("Gregory") shouldBe "Gregory".validNec
    }

    "invalid short length" in {
      PaymentCardValidator.validateCardName("ab") shouldBe CardNameLengthError.invalidNec
    }

    "invalid long length" in {
      PaymentCardValidator.validateCardName("AbulBulbaldiMuhamedov") shouldBe CardNameLengthError.invalidNec
    }

    "invalid symbols" in {
      PaymentCardValidator.validateCardName("@$#!") shouldBe CardNameSpecialSymbolsError.invalidNec
    }
  }

  "CardNumberValidation" - {
    "valid length" in {
      PaymentCardValidator.validateCardNumber("1231231215426154") shouldBe "1231231215426154".validNec
    }

    "invalid short length" in {
      PaymentCardValidator.validateCardNumber("01") shouldBe CardNumberLengthError.invalidNec
    }

    "invalid long length" in {
      PaymentCardValidator.validateCardNumber("12312312154261549") shouldBe CardNumberLengthError.invalidNec
    }

    "invalid symbols I" in {
      PaymentCardValidator.validateCardNumber("-123123121542615") shouldBe CardNumberNumeralsError.invalidNec
    }

    "invalid symbols II" in {
      PaymentCardValidator.validateCardNumber("&$12(#3121542615") shouldBe CardNumberNumeralsError.invalidNec
    }
  }


  "CardExpirationDayValidation" - {
    "valid format" in {
      PaymentCardValidator.validateCardExpirationDay("12/" + yearTwoLastDigits) shouldBe ("12/" + yearTwoLastDigits).validNec
    }

    "invalid short format" in {
      PaymentCardValidator.validateCardExpirationDay("1/"  + yearTwoLastDigits) shouldBe CardExpirationDateFormatError.invalidNec
    }

    "invalid long format" in {
      PaymentCardValidator.validateCardExpirationDay("126/" + yearTwoLastDigits) shouldBe CardExpirationDateFormatError.invalidNec
    }

    "invalid month format" in {
      PaymentCardValidator.validateCardExpirationDay("19/" + yearTwoLastDigits) shouldBe CardExpirationDateMonthNumeralsError.invalidNec
    }

    "invalid year format" in {
      PaymentCardValidator.validateCardExpirationDay("12/" + (java.time.Year.now().getValue - 1).toString.takeRight(2)) shouldBe CardExpirationDateYearNumeralsError.invalidNec
    }
  }

  "CardSecurityCodeValidation" - {
    "valid length" in {
      PaymentCardValidator.validateCardSecurityCode("000") shouldBe "000".validNec
    }

    "invalid short length" in {
      PaymentCardValidator.validateCardSecurityCode("01") shouldBe CardSecurityCodeLengthError.invalidNec
    }

    "invalid long length" in {
      PaymentCardValidator.validateCardSecurityCode("0009") shouldBe CardSecurityCodeLengthError.invalidNec
    }

    "invalid symbols" in {
      PaymentCardValidator.validateCardSecurityCode("$%0") shouldBe CardSecurityCodeNumeralsError.invalidNec
    }
  }

}