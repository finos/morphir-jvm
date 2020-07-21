package com.ms.booksandrecords.commands;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes({
        @JsonSubTypes.Type(value = OpenDeal.class, name = "openDeal"),
        @JsonSubTypes.Type(value = CloseDeal.class, name = "closeDeal")
})
public abstract class DealCmd {
}
