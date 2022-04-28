package morphir.sdk

import java.time.temporal.ChronoUnit

import morphir.sdk.Basics.Int

object LocalTime {

  type LocalTime = java.time.LocalTime

  def addHours(hours: Int)(localTime: LocalTime): LocalTime =
    localTime.plusHours(hours)

  def addMinute(minutes: Int)(localTime: LocalTime): LocalTime =
    localTime.plusMinutes(minutes)

  def addSeconds(seconds: Int)(localTime: LocalTime): LocalTime =
    localTime.plusSeconds(seconds)

  def diffInHours(localTime1: LocalTime)(localTime2: LocalTime): Int =
    ChronoUnit.HOURS.between(localTime1, localTime2).toInt

  def diffInMinutes(localTime1: LocalTime)(localTime2: LocalTime): Int =
    ChronoUnit.MINUTES.between(localTime1, localTime2).toInt

  def diffInSeconds(localTime1: LocalTime)(localTime2: LocalTime): Int =
    ChronoUnit.SECONDS.between(localTime1, localTime2).toInt

}
