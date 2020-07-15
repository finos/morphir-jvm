package com.ms.booksandrecords.processor.factory;

import com.ms.booksandrecords.commands.DealCmd;

public interface AbstractCmdProcessorFactory <T>{
    T create(DealCmd cmd);
}
