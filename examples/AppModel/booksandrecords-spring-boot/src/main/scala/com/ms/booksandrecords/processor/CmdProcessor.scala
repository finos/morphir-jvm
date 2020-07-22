package com.ms.booksandrecords.processor

import com.ms.booksandrecords.repository.DealRepository
import com.ms.booksandrecords.service.DealEvent

trait CmdProcessor {
  def processCmd(dealRepository: DealRepository): DealEvent
}