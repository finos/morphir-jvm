package com.ms.booksandrecords.serviceImpl;

import com.ms.booksandrecords.commands.DealCmd;
import com.ms.booksandrecords.processor.CmdProcessor;
import com.ms.booksandrecords.processor.factory.AbstractCmdProcessorFactory;
import com.ms.booksandrecords.repository.DealRepository;
import com.ms.booksandrecords.service.DealEvent;
import com.ms.booksandrecords.service.ServiceStatefulApp;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class StatefulApp implements ServiceStatefulApp {
    @Autowired
    DealRepository dealRepository;

    @Autowired
    AbstractCmdProcessorFactory<CmdProcessor> factory;


    @Override
    public DealEvent logic(DealCmd dealCmd){

        return process(dealCmd, factory.create(dealCmd));
    }

    public <C> DealEvent process(C command, CmdProcessor<C> processor){
        return processor.processCmd(command, dealRepository);
    }



}
