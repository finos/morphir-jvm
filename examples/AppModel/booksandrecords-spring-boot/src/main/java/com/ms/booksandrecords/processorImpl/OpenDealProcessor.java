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


package com.ms.booksandrecords.processorImpl;

import com.ms.booksandrecords.commands.OpenDeal;
import com.ms.booksandrecords.model.Deal;
import com.ms.booksandrecords.processor.CmdProcessor;
import com.ms.booksandrecords.repository.DealRepository;
import com.ms.booksandrecords.service.DealEvent;
import com.ms.booksandrecords.service.dealevent.DealOpened;
import com.ms.booksandrecords.service.dealevent.DuplicateDeal;
import com.ms.booksandrecords.service.dealevent.InvalidPrice;
import com.ms.booksandrecords.service.dealevent.InvalidQuantity;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
public class OpenDealProcessor implements CmdProcessor <OpenDeal>{

    @Override
    public DealEvent processCmd(OpenDeal openDeal, DealRepository dealRepository) {
        Optional<Deal> dealOptional = dealRepository.findById(openDeal.getId());
        if (dealOptional.isPresent()) {
            return new DuplicateDeal(openDeal.getId());
        }
        else if(openDeal.getPrice() < 0){
            return new InvalidPrice(openDeal.getId(), openDeal.getPrice());
        }
        else if(openDeal.getQuantity() < 0){
            return new InvalidQuantity(openDeal.getId(), openDeal.getQuantity());
        }
        else {
            Deal deal = new Deal(openDeal.getId(), openDeal.getProduct(), openDeal.getPrice(), openDeal.getQuantity());
            dealRepository.save(deal);
            return new DealOpened(openDeal.getId(), openDeal.getProduct(), openDeal.getPrice(), openDeal.getQuantity());
        }
    }
}
