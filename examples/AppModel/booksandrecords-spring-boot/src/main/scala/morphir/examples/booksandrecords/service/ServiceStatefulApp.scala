package morphir.examples.booksandrecords.service

import morphir.examples.booksandrecords.API.{DealCmd, DealEvent}

trait ServiceStatefulApp {
  def logic(dealCmd: DealCmd): DealEvent
}