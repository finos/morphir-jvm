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
