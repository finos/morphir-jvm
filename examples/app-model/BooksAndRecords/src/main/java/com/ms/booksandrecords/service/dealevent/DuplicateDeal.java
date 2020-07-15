package com.ms.booksandrecords.service.dealevent;

import com.ms.booksandrecords.service.DealEvent;

public class DuplicateDeal extends DealEvent {
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    private String id;
    public DuplicateDeal(String id){
        this.id = id;
    }
}
