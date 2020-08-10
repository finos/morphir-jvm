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
