package com.ms.booksandrecords.controller;

import com.ms.booksandrecords.commands.DealCmd;
import com.ms.booksandrecords.service.DealEvent;
import com.ms.booksandrecords.service.ServiceStatefulApp;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class DealController {

    @Autowired
    ServiceStatefulApp serviceStatefulApp;

    @PostMapping(value="/deal", consumes = MediaType.APPLICATION_JSON_VALUE, produces = "application/json")
    public DealEvent processDealCmd(@RequestBody DealCmd dealCmd){
        return serviceStatefulApp.logic(dealCmd);
    }
}
