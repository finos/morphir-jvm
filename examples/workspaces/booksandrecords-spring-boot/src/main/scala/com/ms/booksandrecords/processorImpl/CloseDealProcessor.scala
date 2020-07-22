package com.ms.booksandrecords.processorImpl

import com.ms.booksandrecords.commands.CloseDeal
import com.ms.booksandrecords.processor.CmdProcessor
import com.ms.booksandrecords.repository.DealRepository
import com.ms.booksandrecords.service.dealevent.{DealClosed, DealNotFound}
import org.springframework.stereotype.Component

@Component class CloseDealProcessor(closeDeal: CloseDeal) extends CmdProcessor {
  override def processCmd(dealRepository: DealRepository) = {
    val dealOptional = dealRepository.findById(closeDeal.getId)

    if (dealOptional.isPresent) {
      dealRepository.deleteById(closeDeal.getId)
      new DealClosed(closeDeal.getId)
    }
    else new DealNotFound(closeDeal.getId)
  }
}