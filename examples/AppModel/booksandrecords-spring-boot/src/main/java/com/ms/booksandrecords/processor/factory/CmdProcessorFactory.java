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
