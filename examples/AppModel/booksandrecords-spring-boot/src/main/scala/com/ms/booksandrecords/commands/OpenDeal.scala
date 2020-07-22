package com.ms.booksandrecords.commands

import com.fasterxml.jackson.annotation.JsonProperty
import org.springframework.stereotype.Component

@Component class OpenDeal() extends DealCmd {
  private var id = ""
  private var product = ""
  private var price = .0f
  private var quantity = 0

  def this(@JsonProperty("id") id: String, @JsonProperty("productid") productId: String, @JsonProperty("price") price: Float, @JsonProperty("quantity") quantity: Int) {
    this()
    this.id = id
    product = productId
    this.price = price
    this.quantity = quantity
  }

  def getId = id

  def setId(id: String) = this.id = id

  def getProduct = product

  def setProduct(product: String) = this.product = product

  def getPrice = price

  def setPrice(price: Float) = this.price = price

  def getQuantity = quantity

  def setQuantity(quantity: Int) = this.quantity = quantity
}