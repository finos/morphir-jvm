package com.ms.booksandrecords.commands;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.springframework.stereotype.Component;

@Component
public class CloseDeal extends DealCmd {
    String id;
    public CloseDeal(){}
    public CloseDeal(@JsonProperty("id") String id) {

        this.id = id;
    }
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }
}
