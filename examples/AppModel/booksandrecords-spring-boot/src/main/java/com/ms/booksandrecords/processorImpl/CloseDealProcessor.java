package com.ms.booksandrecords.processorImpl;

import com.ms.booksandrecords.commands.CloseDeal;
import com.ms.booksandrecords.model.Deal;
import com.ms.booksandrecords.processor.CmdProcessor;
import com.ms.booksandrecords.repository.DealRepository;
import com.ms.booksandrecords.service.DealEvent;
import com.ms.booksandrecords.service.dealevent.DealClosed;
import com.ms.booksandrecords.service.dealevent.DealNotFound;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
public class CloseDealProcessor implements CmdProcessor <CloseDeal> {

    @Override
    public DealEvent processCmd(CloseDeal closeDeal, DealRepository dealRepository) {
        Optional<Deal> dealOptional = dealRepository.findById(closeDeal.getId());
        if (dealOptional.isPresent()) {
            dealRepository.deleteById(closeDeal.getId());
            return new DealClosed(closeDeal.getId());
        }
        else {
            return new DealNotFound(closeDeal.getId());
        }
    }
}
