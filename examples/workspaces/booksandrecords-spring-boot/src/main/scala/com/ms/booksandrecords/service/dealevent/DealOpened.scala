package com.ms.booksandrecords.service.dealevent

import com.ms.booksandrecords.service.DealEvent

class DealOpened(var id: String, var product: String, var price: Float, var quantity: Int) extends DealEvent {
  def getId = id

  def setId(id: String) = this.id = id

  def getProduct = product

  def setProduct(product: String) = this.product = product

  def getPrice = price

  def setPrice(price: Float) = this.price = price

  def getQuantity = quantity

  def setQuantity(quantity: Int) = this.quantity = quantity
}