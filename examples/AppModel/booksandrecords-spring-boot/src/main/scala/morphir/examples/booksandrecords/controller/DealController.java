package morphir.examples.booksandrecords.controller;


import morphir.examples.booksandrecords.API;
import morphir.examples.booksandrecords.service.ServiceStatefulApp;
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
    public API.DealEvent processDealCmd(@RequestBody API.DealCmd dealCmd){

        return serviceStatefulApp.logic(dealCmd);
    }
}
