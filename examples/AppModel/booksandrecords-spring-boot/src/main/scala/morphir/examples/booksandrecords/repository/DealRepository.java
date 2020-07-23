package morphir.examples.booksandrecords.repository;

import org.springframework.data.mongodb.repository.MongoRepository;

public interface DealRepository extends MongoRepository<morphir.examples.booksandrecords.model.State.Deal, String>{

}
