package com.ms.booksandrecords.processorImpl

import com.ms.booksandrecords.commands.OpenDeal
import com.ms.booksandrecords.model.Deal
import com.ms.booksandrecords.processor.CmdProcessor
import com.ms.booksandrecords.repository.DealRepository
import com.ms.booksandrecords.service.DealEvent
import com.ms.booksandrecords.service.dealevent.{DealOpened, DuplicateDeal, InvalidPrice, InvalidQuantity}
import org.springframework.stereotype.Component

@Component class OpenDealProcessor (openDeal: OpenDeal) extends CmdProcessor {

  override def processCmd(dealRepository: DealRepository):DealEvent = {
    val dealOptional = dealRepository.findById(openDeal.getId)

    if (dealOptional.isPresent) {
      new DuplicateDeal(openDeal.getId)
    }
    else if (openDeal.getPrice < 0) {
      new InvalidPrice(openDeal.getId, openDeal.getPrice)
    }
    else if (openDeal.getQuantity < 0) {
      new InvalidQuantity(openDeal.getId, openDeal.getQuantity)
    }
    else {
      val deal = new Deal(openDeal.getId, openDeal.getProduct, openDeal.getPrice, openDeal.getQuantity)
      dealRepository.save(deal)
      new DealOpened(openDeal.getId, openDeal.getProduct, openDeal.getPrice, openDeal.getQuantity)
    }
  }
}