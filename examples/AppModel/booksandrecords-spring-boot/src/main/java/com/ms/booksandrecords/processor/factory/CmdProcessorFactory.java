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


package com.ms.booksandrecords.processor.factory;

import com.ms.booksandrecords.commands.DealCmd;
import com.ms.booksandrecords.processor.CmdProcessor;
import com.ms.booksandrecords.processorImpl.CloseDealProcessor;
import com.ms.booksandrecords.processorImpl.OpenDealProcessor;
import org.springframework.stereotype.Component;

@Component
public class CmdProcessorFactory implements AbstractCmdProcessorFactory <CmdProcessor> {
    @Override
    public CmdProcessor create(DealCmd cmd){
        if(cmd.getClass().getSimpleName().equals("OpenDeal")){
            return new OpenDealProcessor();
        }
        else{
            return new CloseDealProcessor();
        }
    }

}
