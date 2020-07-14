package com.ms.booksandrecords.repository;

import com.ms.booksandrecords.model.Deal;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface DealRepository extends MongoRepository<Deal, String>{

}
