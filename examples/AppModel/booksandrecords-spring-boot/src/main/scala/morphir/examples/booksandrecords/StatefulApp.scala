package morphir.examples.booksandrecords.serviceImpl

import morphir.examples.booksandrecords.API
import morphir.examples.booksandrecords.processor.CmdProcessor.processCmd

import morphir.examples.booksandrecords.repository.DealRepository
import morphir.examples.booksandrecords.service.ServiceStatefulApp
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service

@Service class StatefulApp extends ServiceStatefulApp {
  @Autowired private val dealRepository: DealRepository = null

  override def logic(dealCmd: API.DealCmd) =
    processCmd(dealCmd, dealRepository)

}