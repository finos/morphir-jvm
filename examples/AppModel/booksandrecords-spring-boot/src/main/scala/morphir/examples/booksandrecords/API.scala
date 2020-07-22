package morphir.examples.booksandrecords

import org.springframework.stereotype.Component
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

object API {
    type DealId = String
    type ProductId = String
    type Price = BigDecimal
    type Quantity = Int


    // Commands
    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
    @JsonSubTypes({
            @JsonSubTypes.Type(value = classOf[OpenDeal], name = "openDeal"),
            @JsonSubTypes.Type(value = classOf[CloseDeal], name = "closeDeal")
    })
    sealed trait DealCmd

    @Component
    case class CloseDeal(dealId: DealId) extends DealCmd


    @Component
    case class OpenDeal (id: DealId, productId: ProductId, price: Price, quantity: Quantity) extends DealCmd 

    // Events
    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
    @JsonSubTypes({
            @JsonSubTypes.Type(value = classOf[DuplicateDeal], name = "duplicateDeal"),
            @JsonSubTypes.Type(value = classOf[DealNotFound], name = "dealNotFound"),
            @JsonSubTypes.Type(value = classOf[InvalidPrice], name = "invalidPrice"),
            @JsonSubTypes.Type(value = classOf[InvalidQuantity], name = "invalidQuantity")
    })
    sealed trait RejectReason

    case object DuplicateDeal
    case object DealNotFound
    case object InvalidPrice
    case object InvalidQuantity

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
    @JsonSubTypes({
            @JsonSubTypes.Type(value = classOf[DealClosed], name = "dealClosed"),
            @JsonSubTypes.Type(value = classOf[DealNotFound], name = "dealOpened"),
            @JsonSubTypes.Type(value = classOf[DealNotFound], name = "CommandRejected"),
    })
    sealed trait DealEvent

    case class DealOpened(id: DealId, productId: ProductId, price: Price, quantity: Quantity) extends DealEvent
    case class DealClosed(dealId: DealId) extends DealEvent
    case class CommandRejected(dealId: DealId, reason: RejectReason, description: String)
}
