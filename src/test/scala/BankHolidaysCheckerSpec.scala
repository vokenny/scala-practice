import java.time.LocalDate
import java.time.format.DateTimeParseException

import BankHolidaysChecker.{getFileLines, isBankHoliday}
import org.scalatest.FlatSpec

class BankHolidaysCheckerSpec extends FlatSpec {

  val bankHolidays: List[LocalDate] = getFileLines("bank_holidays.csv")
    .map(e => LocalDate.parse(e.replaceAll(",", "")))
    .toList


  "isBankHoliday" should "return true for a given date that is a bank holiday" in {
    assert(isBankHoliday(LocalDate.parse("2020-12-25"), bankHolidays))
    assert(isBankHoliday(LocalDate.parse("2026-05-25"), bankHolidays))
  }

  it should "return false for a given date that is not a bank holiday" in {
    assert(!isBankHoliday(LocalDate.parse("2023-01-02"), bankHolidays))
    assert(!isBankHoliday(LocalDate.parse("2029-08-28"), bankHolidays))
  }

  it should "return false for a given date and an empty list of dates" in {
    assert(!isBankHoliday(LocalDate.parse("2020-12-25"), List()))
    assert(!isBankHoliday(LocalDate.parse("2029-08-28"), Nil))
  }

  it should "throw an DateTimeParseException when an empty LocalDate is given" in {
    assertThrows[DateTimeParseException] {
      isBankHoliday(LocalDate.parse(""), bankHolidays)
    }
  }

  it should "throw an DateTimeParseException when a LocalDate in unrecognised format is given" in {
    assertThrows[DateTimeParseException] {
      isBankHoliday(LocalDate.parse("25-12-2019"), bankHolidays)
    }

    assertThrows[DateTimeParseException] {
      isBankHoliday(LocalDate.parse("12-25-2019"), bankHolidays)
    }

    assertThrows[DateTimeParseException] {
      isBankHoliday(LocalDate.parse("12-25-2019"), bankHolidays)
    }

    assertThrows[DateTimeParseException] {
      isBankHoliday(LocalDate.parse("25th December 2019"), bankHolidays)
    }
  }
}
