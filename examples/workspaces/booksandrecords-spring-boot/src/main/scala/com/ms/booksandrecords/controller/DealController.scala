package com.ms.booksandrecords.controller

import com.ms.booksandrecords.commands.DealCmd
import com.ms.booksandrecords.service.{DealEvent, ServiceStatefulApp}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.MediaType
import org.springframework.web.bind.annotation.{PostMapping, RequestBody, RestController}

@RestController class DealController {

  @Autowired private val serviceStatefulApp: ServiceStatefulApp = null

  @PostMapping(value = Array("/deal"), consumes = Array(MediaType.APPLICATION_JSON_VALUE), produces = Array("application/json"))
  def processDealCmd(@RequestBody dealCmd: DealCmd): DealEvent = {
    serviceStatefulApp.logic(dealCmd)
  }
}