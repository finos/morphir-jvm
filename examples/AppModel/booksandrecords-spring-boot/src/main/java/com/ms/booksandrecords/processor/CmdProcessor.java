package com.ms.booksandrecords.processor;

import com.ms.booksandrecords.repository.DealRepository;
import com.ms.booksandrecords.service.DealEvent;

public interface CmdProcessor <DealCmd>{
    DealEvent processCmd(DealCmd dealCmd, DealRepository dealRepository);
}
