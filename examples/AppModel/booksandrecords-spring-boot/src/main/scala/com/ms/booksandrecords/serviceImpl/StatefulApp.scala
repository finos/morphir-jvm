package com.ms.booksandrecords.serviceImpl

import com.ms.booksandrecords.commands.DealCmd
import com.ms.booksandrecords.processor.CmdProcessor
import com.ms.booksandrecords.processor.factory.AbstractCmdProcessorFactory
import com.ms.booksandrecords.repository.DealRepository
import com.ms.booksandrecords.service.{DealEvent, ServiceStatefulApp}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service

@Service class StatefulApp extends ServiceStatefulApp {
  @Autowired private val dealRepository: DealRepository = null
  @Autowired private val factory: AbstractCmdProcessorFactory[CmdProcessor] = null

  override def logic(dealCmd: DealCmd): DealEvent = process(dealCmd, factory.create(dealCmd))

  def process[T](command: T, processor: CmdProcessor) = processor.processCmd(dealRepository)
}