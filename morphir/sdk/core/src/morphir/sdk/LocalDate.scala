/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package morphir.sdk

import java.time.temporal.ChronoUnit._

import morphir.sdk.Int.Int64

object LocalDate {
  type LocalDate = java.time.LocalDate

  def addDays(count: Long)(date: LocalDate): LocalDate =
    date plusDays count

  def addWeeks(count: Long)(date: LocalDate): LocalDate =
    date plusWeeks count

  def addMonths(count: Long)(date: LocalDate): LocalDate =
    date plusMonths count

  def addYears(count: Long)(date: LocalDate): LocalDate =
    date plusYears count

  def diffInDays(fromDate: LocalDate)(toDate: LocalDate): Int64 =
    DAYS between (fromDate, toDate)

  def diffInWeeks(fromDate: LocalDate)(toDate: LocalDate): Int64 =
    WEEKS between (fromDate, toDate)

  def diffInMonths(fromDate: LocalDate)(toDate: LocalDate): Int64 =
    MONTHS between (fromDate, toDate)

  def diffInYears(fromDate: LocalDate)(toDate: LocalDate): Int64 =
    YEARS between (fromDate, toDate)

  /**
   * Provides a conversion from a `java.time.LocalDate` to a `morphir.sdk.LocalDate.LocalDate`
   */
  implicit def fromJavaTimeLocalDate(
    localDate: java.time.LocalDate
  ): LocalDate =
    localDate
}
