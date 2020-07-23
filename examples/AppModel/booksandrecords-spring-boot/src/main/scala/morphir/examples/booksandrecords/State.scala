package morphir.examples.booksandrecords.model;

import org.springframework.data.annotation.Id
import morphir.examples.booksandrecords.API._

object State {
    case class Deal (
        @Id
        id  : DealId,
        productId   : ProductId,
        price       : Price,
        quantity    : Quantity
    )
}