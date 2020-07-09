package org.morphir.sdk

object LocalDate {
  type LocalDate = java.time.LocalDate

  type Month = java.time.Month

  val Jan: Month = java.time.Month.JANUARY
  val Feb: Month = java.time.Month.FEBRUARY
  val Mar: Month = java.time.Month.MARCH
  val Apr: Month = java.time.Month.APRIL
  val May: Month = java.time.Month.MAY
  val Jun: Month = java.time.Month.JUNE
  val Jul: Month = java.time.Month.JULY
  val Aug: Month = java.time.Month.AUGUST
  val Sep: Month = java.time.Month.SEPTEMBER
  val Oct: Month = java.time.Month.OCTOBER
  val Nov: Month = java.time.Month.NOVEMBER
  val Dec: Month = java.time.Month.DECEMBER

  type DayOfWeek = java.time.DayOfWeek
  val Sunday: DayOfWeek    = java.time.DayOfWeek.SUNDAY
  val Monday: DayOfWeek    = java.time.DayOfWeek.MONDAY
  val Tuesday: DayOfWeek   = java.time.DayOfWeek.TUESDAY
  val Wednesday: DayOfWeek = java.time.DayOfWeek.WEDNESDAY
  val Thursday: DayOfWeek  = java.time.DayOfWeek.THURSDAY
  val Friday: DayOfWeek    = java.time.DayOfWeek.FRIDAY
  val Saturday: DayOfWeek  = java.time.DayOfWeek.SATURDAY

}
