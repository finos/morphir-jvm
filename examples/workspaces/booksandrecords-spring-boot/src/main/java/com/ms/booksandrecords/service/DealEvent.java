package com.ms.booksandrecords.service;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.ms.booksandrecords.service.dealevent.DealClosed;
import com.ms.booksandrecords.service.dealevent.DealNotFound;

//Add this property to the json to show this data in the output
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes({
        @JsonSubTypes.Type(value = DealClosed.class, name = "dealClosed"),
        @JsonSubTypes.Type(value = DealNotFound.class, name = "dealNotFound"),
        @JsonSubTypes.Type(value = DealNotFound.class, name = "dealOpened"),
        @JsonSubTypes.Type(value = DealNotFound.class, name = "duplicateDeal"),
        @JsonSubTypes.Type(value = DealNotFound.class, name = "invalidPrice"),
        @JsonSubTypes.Type(value = DealNotFound.class, name = "invalidQuantity")
})
public abstract class DealEvent {
}