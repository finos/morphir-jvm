/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/


package morphir.examples.booksandrecords

import com.fasterxml.jackson.annotation.{JsonAutoDetect, JsonProperty, JsonSubTypes, JsonTypeInfo}
import org.springframework.context.annotation.Bean;


object API {
  type DealId = String
  type ProductId = String
  type Price = Float
  type Quantity = Int


  // Commands
  @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
  @JsonSubTypes(Array
  (
    new JsonSubTypes.Type(value = classOf[OpenDeal], name = "openDeal"),
    new JsonSubTypes.Type(value = classOf[CloseDeal], name = "closeDeal")
  ))
  sealed trait DealCmd

  @Bean
  case class CloseDeal(@JsonProperty("dealId") dealId: DealId) extends DealCmd


  @Bean
  case class OpenDeal(@JsonProperty("dealId") dealId: DealId, @JsonProperty("productId") productId: ProductId, @JsonProperty("price") price: Price, @JsonProperty("quantity") quantity: Quantity) extends DealCmd

  // Events
  @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
  sealed trait RejectReason

  case object DuplicateDeal extends RejectReason

  case object DealNotFound extends RejectReason

  case object InvalidPrice extends RejectReason

  case object InvalidQuantity extends RejectReason

  @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
  @JsonSubTypes(Array
  (
    new JsonSubTypes.Type(value = classOf[DealClosed], name = "dealClosed"),
    new JsonSubTypes.Type(value = classOf[DealOpened], name = "dealOpened"),
    new JsonSubTypes.Type(value = classOf[CommandRejected], name = "CommandRejected")
  ))
  @JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
  sealed trait DealEvent

  case class DealOpened(  id: DealId, productId: ProductId, price: Price, quantity: Quantity) extends DealEvent

  case class DealClosed(dealId: DealId) extends DealEvent

  case class CommandRejected(dealId: DealId, reason: RejectReason, description: String) extends DealEvent
}
