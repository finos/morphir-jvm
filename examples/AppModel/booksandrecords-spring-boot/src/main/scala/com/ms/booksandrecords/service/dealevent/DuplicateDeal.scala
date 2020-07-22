package com.ms.booksandrecords.service.dealevent

import com.ms.booksandrecords.service.DealEvent

class DuplicateDeal(var id: String) extends DealEvent {
  def getId = id

  def setId(id: String) = this.id = id
}