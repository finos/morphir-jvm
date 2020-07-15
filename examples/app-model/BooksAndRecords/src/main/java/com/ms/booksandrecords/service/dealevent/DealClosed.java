package com.ms.booksandrecords.service.dealevent;

import com.ms.booksandrecords.service.DealEvent;


public class DealClosed extends DealEvent {
    private String id;
    public DealClosed(String id){
        this.id = id;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }
}
