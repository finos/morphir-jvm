package morphir.examples.booksandrecords.processor

import morphir.examples.booksandrecords.API.{CloseDeal, DealCmd, DealEvent, OpenDeal}
import morphir.examples.booksandrecords.Logic._
import morphir.examples.booksandrecords.repository.DealRepository

object CmdProcessor {
  def processCmd(cmd: DealCmd, dealRepository: DealRepository): DealEvent = {
    cmd match {
      case cmd: OpenDeal => openDeal(cmd, dealRepository)
      case cmd: CloseDeal => closeDeal(cmd, dealRepository)
    }
  }
}