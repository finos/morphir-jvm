package morphir.examples.booksandrecords;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.core.convert.converter.Converter;
import org.springframework.data.mongodb.core.convert.CustomConversions;

@SpringBootApplication
public class BooksAndRecordsApplication {

	public static void main(String[] args) {

		SpringApplication.run(BooksAndRecordsApplication.class, args);
	}

}
