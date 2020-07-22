package com.ms.booksandrecords.model;

import org.springframework.data.annotation.Id;
import com.ms.booksandrecords.API.*;

object State {
    case class Deal (
        @Id dealId  : DealId, 
        productId   : ProductId,
        price       : Price, 
        quantity    : Quantity
    )
}