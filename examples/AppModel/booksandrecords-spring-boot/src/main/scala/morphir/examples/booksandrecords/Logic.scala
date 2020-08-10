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

import morphir.examples.booksandrecords.API._
import morphir.examples.booksandrecords.model.State.Deal
import morphir.examples.booksandrecords.repository.DealRepository


object Logic {
    def openDeal(command: OpenDeal, dealRepository: DealRepository) : DealEvent = {
        val dealOptional = dealRepository.findById(command.dealId)

        if (dealOptional.isPresent) {
            new CommandRejected(command.dealId, DuplicateDeal, "Duplicated")
        }
        else if (command.price < 0) {
            new CommandRejected(command.dealId, InvalidPrice, "Wrong Price")
        }
        else if (command.quantity < 0) {
            new CommandRejected(command.dealId, InvalidQuantity, "Wrong Quantity")
        }
        else {
            val deal = new Deal(command.dealId, command.productId, command.price, command.quantity)
            dealRepository.save(deal)
            new DealOpened(command.dealId, command.productId, command.price, command.quantity)
        }
    }

    def closeDeal(command: CloseDeal, dealRepository: DealRepository) : DealEvent = {
        val dealOptional = dealRepository.findById(command.dealId)

        if (dealOptional.isPresent) {
            dealRepository.deleteById(command.dealId)
            new DealClosed(command.dealId)
        }
        else new CommandRejected(command.dealId, DealNotFound, "Deal Not Found")
    }
}