package com.ms.booksandrecords.commands

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes(Array(
  new JsonSubTypes.Type(value = classOf[OpenDeal], name = "openDeal"),
    new JsonSubTypes.Type(value = classOf[CloseDeal], name = "closeDeal")
)
)
trait  DealCmd {
}