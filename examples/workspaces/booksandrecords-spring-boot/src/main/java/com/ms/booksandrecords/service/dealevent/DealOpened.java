package com.ms.booksandrecords.service.dealevent;

import com.ms.booksandrecords.service.DealEvent;

public class DealOpened extends DealEvent {
    private String id;
    private String product;
    private float price;
    private int quantity;
    public DealOpened(String id, String product, float price, int quantity){
        this.id = id;
        this.product = product;
        this.price = price;
        this.quantity = quantity;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getProduct() {
        return product;
    }

    public void setProduct(String product) {
        this.product = product;
    }

    public float getPrice() {
        return price;
    }

    public void setPrice(float price) {
        this.price = price;
    }

    public int getQuantity() {
        return quantity;
    }

    public void setQuantity(int quantity) {
        this.quantity = quantity;
    }
}
