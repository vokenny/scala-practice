import java.io.File
import java.time.LocalDate

import scala.io.Source

object BankHolidaysChecker extends App {

  def filePath(fileName: String): String = new File(s"src/main/scala/resources/$fileName").getCanonicalPath

  def getFileLines(fileName: String): Iterator[String] = Source.fromFile(filePath(fileName)).getLines()

  val bankHolidays: List[LocalDate] = getFileLines("bank_holidays.csv")
    .map(e => LocalDate.parse(e.replaceAll(",", "")))
    .toList

  def isBankHoliday(date: LocalDate, bankHolidaysList: List[LocalDate]): Boolean = {
    if (bankHolidaysList.nonEmpty && bankHolidaysList.contains(date)) true else false
  }

  println(isBankHoliday(LocalDate.parse("2020-12-25"), bankHolidays))
  println(isBankHoliday(LocalDate.parse("2020-12-31"), bankHolidays))
}
