package com.ms.booksandrecords.service.dealevent;

import com.ms.booksandrecords.service.DealEvent;

public class InvalidPrice extends DealEvent {
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public float getPrice() {
        return price;
    }

    public void setPrice(float price) {
        this.price = price;
    }

    private String id;
    private float price;

    public InvalidPrice(String id, float price) {
        this.id = id;
        this.price = price;
    }
}
