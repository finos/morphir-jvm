package com.ms.booksandrecords.service;

import com.ms.booksandrecords.commands.DealCmd;

public interface  ServiceStatefulApp {

    DealEvent logic(DealCmd dealCmd);
}
