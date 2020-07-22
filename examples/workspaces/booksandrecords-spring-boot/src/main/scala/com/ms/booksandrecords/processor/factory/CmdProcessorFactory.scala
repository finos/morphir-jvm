package com.ms.booksandrecords.processor.factory

import com.ms.booksandrecords.commands.{CloseDeal, DealCmd, OpenDeal}
import com.ms.booksandrecords.processor.CmdProcessor
import com.ms.booksandrecords.processorImpl.{CloseDealProcessor, OpenDealProcessor}
import org.springframework.stereotype.Component

@Component class CmdProcessorFactory extends AbstractCmdProcessorFactory[CmdProcessor] {
  override def create(cmd: DealCmd)  =
    cmd match {
      case cmd: OpenDeal => new OpenDealProcessor(cmd)
      case cmd: CloseDeal => new CloseDealProcessor(cmd)
    }
}