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