package com.ms.booksandrecords.commands;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.springframework.stereotype.Component;


@Component
public class OpenDeal extends DealCmd {
    private String id, product;
    private float price;
    private int quantity;

    public OpenDeal(){ }
    public OpenDeal(@JsonProperty("id") String id, @JsonProperty("productid") String productId,
                    @JsonProperty("price") float price, @JsonProperty("quantity") int quantity) {
        this.id = id;
        product = productId;
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
