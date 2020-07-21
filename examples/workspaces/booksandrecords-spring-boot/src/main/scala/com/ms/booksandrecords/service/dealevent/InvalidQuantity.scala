package com.ms.booksandrecords.service.dealevent

import com.ms.booksandrecords.service.DealEvent

class InvalidQuantity(var id: String, var quantity: Int) extends DealEvent {
  def getId = id

  def setId(id: String) = this.id = id

  def getQuantity = quantity

  def setQuantity(quantity: Int) = this.quantity = quantity
}