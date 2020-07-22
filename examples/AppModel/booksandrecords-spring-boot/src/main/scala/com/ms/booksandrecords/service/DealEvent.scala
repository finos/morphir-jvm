package com.ms.booksandrecords.service

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}
import com.ms.booksandrecords.service.dealevent.{DealClosed, DealNotFound}

//Add this property to the json to show this data in the output
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes(Array
(new JsonSubTypes.Type(value = classOf[DealClosed], name = "dealClosed"),
  new JsonSubTypes.Type(value = classOf[DealNotFound], name = "dealNotFound"),
  new JsonSubTypes.Type(value = classOf[DealNotFound], name = "dealOpened"),
  new JsonSubTypes.Type(value = classOf[DealNotFound], name = "duplicateDeal"),
  new JsonSubTypes.Type(value = classOf[DealNotFound], name = "invalidPrice"),
  new JsonSubTypes.Type(value = classOf[DealNotFound], name = "invalidQuantity")))
abstract class DealEvent {

}