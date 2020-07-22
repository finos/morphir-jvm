package com.ms.booksandrecords.service.dealevent

import com.ms.booksandrecords.service.DealEvent

class InvalidPrice(var id: String, var price: Float) extends DealEvent {
  def getId = id

  def setId(id: String) = this.id = id

  def getPrice = price

  def setPrice(price: Float) = this.price = price
}