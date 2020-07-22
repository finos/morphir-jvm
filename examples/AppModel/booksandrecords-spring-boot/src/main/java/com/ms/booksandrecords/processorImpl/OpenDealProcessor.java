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
