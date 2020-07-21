package com.ms.booksandrecords.commands

import com.fasterxml.jackson.annotation.JsonProperty
import org.springframework.stereotype.Component

@Component class CloseDeal() extends DealCmd {
  var id = ""

  def this(@JsonProperty("id") id: String) {
    this()
    this.id = id
  }

  def getId = id

  def setId(id: String) = this.id = id
}