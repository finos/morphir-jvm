/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/


package com.ms.booksandrecords.statefulapp;

import com.ms.booksandrecords.commands.CloseDeal;
import com.ms.booksandrecords.commands.DealCmd;
import com.ms.booksandrecords.commands.OpenDeal;
import com.ms.booksandrecords.model.Deal;
import com.ms.booksandrecords.repository.DealRepository;
import com.ms.booksandrecords.service.DealEvent;
import com.ms.booksandrecords.service.ServiceStatefulApp;
import com.ms.booksandrecords.service.dealevent.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.util.Assert;

import java.util.Optional;

@SpringBootTest
class StatefulAppTest {

    @Autowired
    ServiceStatefulApp serviceStatefulApp;

    @MockBean
    DealRepository dealRepository;

    @BeforeEach
    public void setUp() {
        Mockito.<Optional<Deal>>when(dealRepository.findById("existingId")).
                thenReturn(Optional.of((Deal) new Deal("existingId", "product1", 1, 10)));
        Mockito.<Optional<Deal>>when(dealRepository.findById("notExistingId")).
                thenReturn(Optional.empty());
    }

    @Test
    void whenDealNotNullAndOpenDealExpectDuplicateDeal() {
        DealCmd openDeal = new OpenDeal("existingId", "product1", 1, 10);

        DealEvent dealEvent = serviceStatefulApp.logic(openDeal);

        Assert.isInstanceOf(DuplicateDeal.class, dealEvent);
        Assert.isTrue(((DuplicateDeal)dealEvent).getId() == "existingId", "The Id returned is not valid");
    }

    @Test
    void whenDealNotNullAndCloseDealExpectDealClosed() {
        DealCmd closeDeal = new CloseDeal("existingId");
        DealEvent dealEvent = serviceStatefulApp.logic(closeDeal);

        Assert.isInstanceOf(DealClosed.class, dealEvent);
        Assert.isTrue(((DealClosed)dealEvent).getId() =="existingId", "The id of the event is not the same");
    }

    @Test
    void whenDealNullAndOpenDealExpectAndPriceBelow0ExpectInvalidPrice() {
        DealCmd openDeal = new OpenDeal("notExistingId", "product1", -25, 10);
        DealEvent dealEvent = serviceStatefulApp.logic(openDeal);

        Assert.isInstanceOf(InvalidPrice.class, dealEvent);

        Assert.isTrue(((InvalidPrice)dealEvent).getId() =="notExistingId", "The id of the event is not the same");
        Assert.isTrue(((InvalidPrice)dealEvent).getPrice() == -25, "The price is not the same");
    }

    @Test
    void whenDealNullAndOpenDealExpectAndQuantityBelow0ExpectInvalidQuantity() {
        DealCmd openDeal = new OpenDeal("notExistingId", "product1", 25, -10);
        DealEvent dealEvent = serviceStatefulApp.logic(openDeal);

        Assert.isInstanceOf(InvalidQuantity.class, dealEvent);

        Assert.isTrue(((InvalidQuantity)dealEvent).getId() =="notExistingId", "The id of the event is not the same");
        Assert.isTrue(((InvalidQuantity)dealEvent).getQuantity() == -10, "The quantity is not the same");
    }

    @Test
    void whenDealNullAndOpenDealExpectDealOpened() {
        DealCmd openDeal = new OpenDeal("notExistingId", "product1", 25, 100);
        DealEvent dealEvent = serviceStatefulApp.logic(openDeal);

        Assert.isInstanceOf(DealOpened.class, dealEvent);
        Assert.isTrue(((DealOpened)dealEvent).getId() == "notExistingId", "The id is invalid");

        Assert.isTrue(((DealOpened)dealEvent).getProduct() == "product1", "The productid is invalid");
        Assert.isTrue(((DealOpened)dealEvent).getPrice() == 25, "The price is invalid");
        Assert.isTrue(((DealOpened)dealEvent).getQuantity() == 100, "The quantity is invalid");
    }

    @Test
    void whenDealNullAndCloseDealExpectDealNotFound() {
        DealCmd closeDeal = new CloseDeal("notExistingId");
        DealEvent dealEvent = serviceStatefulApp.logic(closeDeal);

        Assert.isInstanceOf(DealNotFound.class, dealEvent);
        Assert.isTrue(((DealNotFound)dealEvent).getId() == "notExistingId", "The id is invalid");
    }
}