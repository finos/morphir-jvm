package com.ms.booksandrecords.service.dealevent;

import com.ms.booksandrecords.service.DealEvent;

public class DealNotFound extends DealEvent {
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    private String id;
    public DealNotFound(String id){
        this.id = id;
    }
}
