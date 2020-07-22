package com.ms.booksandrecords.repository

import com.ms.booksandrecords.model.Deal
import org.springframework.data.mongodb.repository.MongoRepository

trait DealRepository extends MongoRepository[Deal, String] {}